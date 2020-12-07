module Typechecker (typecheck, TypecheckResult, runTypechecker) where

import Control.Monad (foldM, mapM)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (MonadReader (ask), ReaderT, lift, runReaderT)
import Control.Monad.Writer (MonadWriter (tell), runWriterT)
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import ExpressionType (typeOfExpression)
import Schema (accessPossibleTys, insertSchemaPath, narrowDiscUnion, removeSchemaPath, updateSchemaTy)
import Types (AST (..), Accumulator (..), BSON (..), BSONType (..), Context, Exception, Expression (..), FieldPath (..), Index (..), Op (..), SchemaTy (..), Stage (..))
import Utils (fromBsonType, isSubtype, toBsonType, withErr, flattenSchemaTy)

type TypecheckResult = ReaderT Context Exception


runTypechecker :: AST -> SchemaTy -> Map.Map String SchemaTy -> Either String (SchemaTy, [String])
runTypechecker p sch db =
  runIdentity (runExceptT (runWriterT (runReaderT (typecheck p sch) db)))

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
      foldM (\acc fp -> lift $ removeSchemaPath fp acc) sch (map reverse l)
    else do
      res <- lift $ processInclusions m (toBsonType sch) sch
      lift $ fromBsonType res
  where
    collectRemovals :: Map.Map String Expression -> FieldPath -> Exception [FieldPath]
    collectRemovals m fp =
      foldM
        ( \acc (k, v) ->
            case v of
              Inclusion False -> return $ (ObjectIndex k : fp) : acc
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
processStage (Group groupByExpr accumulations) schema = do
  idType <- lift $ typeOfExpression schema groupByExpr
  accTypes <- lift $ mapM (\(key, acc, exp) -> (,,) key acc <$> typeOfExpression schema exp) accumulations
  returnTypes <- Map.fromList <$> mapM (\(key, acc, expT) -> (,) key <$> getAccReturnT acc expT) accTypes
  let newSchema = Map.insert "_id" idType returnTypes
  return $ S $ Set.singleton newSchema

-- TODO: Add in accumulator types.
getAccReturnT :: Accumulator -> BSONType -> TypecheckResult BSONType
getAccReturnT _ _ = throwError "Not proper argument type for accumulator."

typecheck :: AST -> SchemaTy -> TypecheckResult SchemaTy
typecheck (Pipeline []) ty = return ty
typecheck (Pipeline (hd : tl)) ty = do
  nextSch <- processStage hd ty
  cleanSch <- lift $ flattenSchemaTy nextSch
  typecheck (Pipeline tl) cleanSch
