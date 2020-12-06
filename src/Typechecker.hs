module Typechecker (typecheck) where

import Control.Monad (foldM, mapM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader (ask), ReaderT, lift)
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import ExpressionType (typeOfExpression)
import Schema (accessPossibleTys, insertSchemaPath, removeSchemaPath, updateSchemaTy)
import Types (Accumulator, AST (..), BSONType (..), Context, Exception, Expression (..), FieldPath (..), Index (..), SchemaTy (..), Stage (..))
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
  if exclude
    then do
      l <- lift $ collectRemovals m []
      foldM
        ( \acc (fp, ty) ->
            case ty of
              Nothing -> lift $ removeSchemaPath fp acc
              Just newTy -> lift $ insertSchemaPath fp newTy acc
        )
        sch
        l
    else do
      res <- lift $ processInclusions m (toBsonType sch) sch
      lift $ fromBsonType res
  where
    collectRemovals :: Map.Map String Expression -> FieldPath -> Exception [(FieldPath, Maybe BSONType)]
    collectRemovals m fp =
      foldM
        ( \acc (k, v) ->
            case v of
              Inclusion False -> return $ (ObjectIndex k : fp, Nothing) : acc
              EObject nxtProjExp -> do
                res <- collectRemovals nxtProjExp (ObjectIndex k : fp)
                return $ res ++ acc
              _ -> throwError "Not possible"
        )
        []
        (Map.toList m)
    processInclusions :: Map.Map String Expression -> BSONType -> SchemaTy -> Exception BSONType
    processInclusions projExp (TSum s) baseSch =
      TSum
        <$> foldM
          ( \acc sch -> do
              newSch <- processInclusions projExp sch baseSch
              return $ Set.insert newSch acc
          )
          Set.empty
          s
    processInclusions projExp (TObject m) baseSch =
      TObject
        <$> foldM
          ( \acc (k, v) -> do
              ogTy <- withErr (m Map.!? k) "Trying to project field that does not exist"
              case v of
                Inclusion True ->
                  return $ Map.insert k ogTy acc
                EObject nxtProjExp -> do
                  res <- processInclusions nxtProjExp ogTy baseSch
                  return $ Map.insert k res acc
                exp -> do
                  ty <- typeOfExpression baseSch exp
                  return $ Map.insert k ty acc
          )
          Map.empty
          (Map.toList projExp)
    processInclusions _ _ _ = undefined


processStage (Group groupByExpr accumulations) schema = do
  idType <- lift $ typeOfExpression schema groupByExpr
  accTypes <- lift $ mapM (\(a,b,c) -> (,,) a b <$> typeOfExpression schema c) accumulations
  newTypes <- Map.fromList <$> mapM (\(key, acc, rT) -> (,) key <$> getAccReturnT acc rT) accTypes
  let newSchema = Map.insert "_id" idType newTypes
  return $ S $ Set.singleton newSchema

processStage _ _ = undefined


-- TODO: Add in accumulator types.
getAccReturnT :: Accumulator -> BSONType -> TypecheckResult BSONType
getAccReturnT _ _ = throwError "Not proper argument type for accumulator."

typecheck :: AST -> SchemaTy -> TypecheckResult SchemaTy
typecheck (Pipeline []) ty = return ty
typecheck (Pipeline (hd : tl)) ty = do
  nextSch <- processStage hd ty
  typecheck (Pipeline tl) nextSch

