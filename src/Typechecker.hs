module Typechecker (typecheck) where

import Control.Monad (foldM, mapM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader (ask), ReaderT, lift)
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Schema (accessPossibleTys, insertSchemaPath, removeSchemaPath, updateSchemaTy)
import Types (AST (..), BSONType (..), Context, Expression (..), FieldPath (..), Index (..), SchemaTy (..), Stage (..))
import Utils (fromBsonType, toBsonType, withErr)

type TypecheckResult = ReaderT Context (ExceptT String Identity)

-- isSubtype t1 t2 checks if t1 <: t2
isSubtype :: BSONType -> BSONType -> Bool
isSubtype (TConst _) TStr = True
isSubtype (TArray ty1) (TArray ty2) = isSubtype ty1 ty2
isSubtype (TSum s1) (TSum s2) =
  all
    ( \ty ->
        any (`isSubtype` ty) s1
    )
    s2
isSubtype (TObject m1) (TObject m2) =
  Map.foldrWithKey
    ( \k v acc ->
        case m1 Map.!? k of
          Just v' -> isSubtype v' v && acc
          Nothing -> False
    )
    True
    m2
isSubtype t1 t2
  | t1 == t2 = True
  | otherwise = False

-- Throws an error if mixture of exclusion with other expressions
isAllExclusion :: Expression -> ExceptT String Identity Bool
isAllExclusion (FP _) = return False
isAllExclusion (Inclusion b) = return $ not b
isAllExclusion (Lit _) = return False
isAllExclusion (EArray _) = return False
isAllExclusion (Application _ _) = return False
isAllExclusion (EObject m) = do
  res <- mapM (\(_, v) -> isAllExclusion v) (Map.toList m)
  if all (== True) res
    then return True
    else
      if all (== False) res
        then return False
        else throwError "Mixing exclusion with other expressions not allowed"

processStage :: Stage -> SchemaTy -> TypecheckResult SchemaTy
processStage (Unwind fp) sch =
  lift $
    updateSchemaTy
      fp
      ( \ty -> case ty of
          TArray x -> return x
          _ -> throwError "Cannot unwind non-array type"
      )
      sch
processStage (Lookup foreignCol localFp foreignFp as) sch = do
  ctx <- ask
  foreignSch <- lift $ withErr (ctx Map.!? foreignCol) "No such foreign collection"
  localTys <- lift $ accessPossibleTys localFp sch
  foreignTys <- lift $ accessPossibleTys foreignFp foreignSch
  if length localTys == 1 && length foreignTys == 1
    then case (localTys, foreignTys) of
      ([lty], [fty]) ->
        if isSubtype lty fty || isSubtype fty lty
          then
            let (S fsl, S lsl) = (foreignSch, sch)
             in return (S (Set.fromList [Map.insert as (TObject fm) lm | fm <- Set.toList fsl, lm <- Set.toList lsl]))
          else throwError "Local and foreign lookup fields do not match"
      _ -> throwError "not possible"
    else throwError "Local and foreign lookup fields do not match"
processStage (Facet m) sch = do
  newTy <-
    mapM
      ( \(k, v) -> do
          facetTy <- typecheck v sch
          return (k, toBsonType facetTy)
      )
      (Map.toList m)
  return $ S (Set.fromList [Map.fromList newTy])
-- support short-hand field path accesses?
processStage (Project m) sch = do
  exclude <- lift $ isAllExclusion (EObject m)
  l <- collectModifications m sch exclude []
  foldM
    ( \acc (fp, ty) ->
        case ty of
          Nothing -> lift $ removeSchemaPath fp acc
          Just newTy -> lift $ insertSchemaPath fp newTy acc
    )
    sch
    l
  where
    collectModifications :: Map.Map String Expression -> SchemaTy -> Bool -> FieldPath -> TypecheckResult [(FieldPath, Maybe BSONType)]
    collectModifications m sch True fp =
      foldM
        ( \acc (k, v) ->
            case v of
              Inclusion False -> return $ (ObjectIndex k : fp, Nothing) : acc
              EObject nxtProjExp -> do
                res <- collectModifications nxtProjExp sch True (ObjectIndex k : fp)
                return $ res ++ acc
              _ -> throwError "Not possible"
        )
        []
        (Map.toList m)
    collectModifications m sch False fp =
      undefined
    collectModifications _ _ _ _ = undefined

-- sty <- helper m (toBsonType sch) sch exclude
-- lift $ fromBsonType sty
-- where
--   helper :: Map.Map String Expression -> BSONType -> SchemaTy -> Bool -> TypecheckResult BSONType
--   helper projExp (TSum s) rootSch exclude =
--     TSum
--       <$> foldM
--         ( \acc sch -> do
--             newSch <- helper projExp sch rootSch exclude
--             return $ Set.insert newSch acc
--         )
--         Set.empty
--         s
--   helper (projExp) (TObject tym) rootSch True =
--     TObject
--       <$> foldM
--         ( \acc (k, v) -> do
--             schVal <- lift $ withErr (tym Map.!? k) "Cannot find field-path in object"
--             case v of
--               EObject em -> undefined
--               Inclusion False -> undefined
--               _ -> throwError "not possible"
--         )
--         tym
--         (Map.toList projExp)
--   helper (projExp) (TObject tym) rootSch False = undefined
--   helper projExp _ _ _ = throwError "Invalid projection traversal"
processStage _ _ = undefined

typecheck :: AST -> SchemaTy -> TypecheckResult SchemaTy
typecheck (Pipeline []) ty = return ty
typecheck (Pipeline (hd : tl)) ty = do
  nextSch <- processStage hd ty
  typecheck (Pipeline tl) nextSch
