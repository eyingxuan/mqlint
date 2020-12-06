module Typechecker (typecheck, TypecheckResult) where

import Control.Monad (foldM, mapM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader (ask), ReaderT, lift)
import Control.Monad.Writer (MonadWriter (tell))
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import ExpressionType (typeOfExpression)
import Schema (accessPossibleTys, insertSchemaPath, narrowDiscUnion, removeSchemaPath, updateSchemaTy)
import Types (AST (..), BSON (..), BSONType (..), Context, Exception, Expression (..), FieldPath (..), Index (..), Op (..), SchemaTy (..), Stage (..))
import Utils (fromBsonType, isSubtype, toBsonType, withErr)

type TypecheckResult = ReaderT Context Exception

-- Throws an error if mixture of exclusion with other expressions
isAllExclusion :: Expression -> Exception Bool
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
    processInclusions _ _ _ = throwError "Invalid projection"
processStage (Match exp) sch = do
  expTy <- lift $ typeOfExpression sch exp
  if expTy /= TBool
    then throwError "Match expr of invalid type"
    else case exp of
      Application Eq [FP fp, Lit (Str s)] ->
        lift $ narrowDiscUnion fp (== s) sch
      _ -> return sch
processStage _ _ = undefined

typecheck :: AST -> SchemaTy -> TypecheckResult SchemaTy
typecheck (Pipeline []) ty = return ty
typecheck (Pipeline (hd : tl)) ty = do
  nextSch <- processStage hd ty
  typecheck (Pipeline tl) nextSch
