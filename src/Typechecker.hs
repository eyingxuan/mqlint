module Typechecker (typecheck, TypecheckResult, runTypechecker) where

import Control.Monad (foldM, mapM)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (MonadReader (ask), ReaderT, lift, runReaderT)
import Control.Monad.Writer (MonadWriter (tell), runWriterT)
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import ExpressionType (typeOfExpression)
import Printing (oneLine)
import Schema (accessPossibleTys, narrowDiscUnion, removeSchemaPath, updateSchemaTy)
import qualified Text.PrettyPrint as PP
import Types
  ( AST (..),
    Accumulator (..),
    BSON (..),
    BSONType (..),
    Contextual (..),
    Expression (..),
    FieldPath (..),
    Index (..),
    Op (..),
    SchemaTy (..),
    Stage (..),
    TypecheckResult,
  )
import Utils (flattenBSONType, flattenSchemaTy, fromBsonType, isSubtype, throwErrorWithContext, toBsonType, withErr)

runTypechecker :: AST -> SchemaTy -> Map.Map String SchemaTy -> Either String (SchemaTy, [String])
runTypechecker p sch db =
  runIdentity (runExceptT (runWriterT (runReaderT (typecheck p sch) (db, id))))

-- Throws an error if mixture of exclusion with other expressions
isAllExclusion :: Expression -> TypecheckResult Bool
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
        else throwErrorWithContext "Mixing exclusion with other expressions not allowed"

processStage :: Stage -> SchemaTy -> TypecheckResult SchemaTy
processStage (Unwind fp) sch =
  withContext
    ( updateSchemaTy
        fp
        ( \ty -> case ty of
            TArray x -> return x
            _ -> throwErrorWithContext "Cannot unwind non-array type"
        )
        sch
    )
    (PP.text ("Unwinding field path: " ++ oneLine (FP fp)))
processStage (Lookup foreignCol localFp foreignFp as) sch = do
  (ctx, _) <- ask
  foreignSch <- withErr (ctx Map.!? foreignCol) "No such foreign collection"
  localTys <- Set.toList . Set.fromList <$> withContext (accessPossibleTys localFp sch) (PP.text ("Accessing local field path: " ++ oneLine (FP localFp)))
  foreignTys <- Set.toList . Set.fromList <$> withContext (accessPossibleTys foreignFp foreignSch) (PP.text ("Accessing foreign field path: " ++ oneLine (FP foreignFp)))
  if length localTys == 1 && length foreignTys == 1
    then case (localTys, foreignTys) of
      ([lty], [fty]) ->
        if isSubtype lty fty || isSubtype fty lty
          then
            let (S fsl, S lsl) = (foreignSch, sch)
             in return (S (Set.fromList [Map.insert as (TArray $ TSum (Set.map TObject fsl)) lm | lm <- Set.toList lsl]))
          else throwErrorWithContext "Local and foreign lookup fields do not match"
      _ -> throwErrorWithContext "not possible"
    else throwErrorWithContext "Local and foreign lookup fields do not match"
processStage (Facet m) sch = do
  newTy <-
    mapM
      ( \(k, v) -> do
          facetTy <- withContext (typecheck v sch) (PP.text ("Typechecking field " ++ k ++ " with pipeline " ++ oneLine v))
          return (k, toBsonType facetTy)
      )
      (Map.toList m)
  return $ S (Set.fromList [Map.fromList newTy])

-- support short-hand field path accesses?
processStage (Project ogM) sch =
  let m = Map.delete "_id" ogM
   in do
        exclude <- isAllExclusion (EObject m)
        if exclude
          then do
            l <- collectRemovals m []
            foldM
              ( \acc fp ->
                  withContext
                    (removeSchemaPath fp acc)
                    (PP.text ("Removing field path " ++ oneLine (FP fp)))
              )
              sch
              (map reverse l)
          else do
            res <- processInclusions m (toBsonType sch) sch
            fromBsonType res
  where
    collectRemovals :: Map.Map String Expression -> FieldPath -> TypecheckResult [FieldPath]
    collectRemovals m fp =
      foldM
        ( \acc (k, v) ->
            case v of
              Inclusion False -> return $ (ObjectIndex k : fp) : acc
              EObject nxtProjExp -> do
                res <- collectRemovals nxtProjExp (ObjectIndex k : fp)
                return $ res ++ acc
              _ -> throwErrorWithContext "Not possible"
        )
        []
        (Map.toList m)
    processInclusions :: Map.Map String Expression -> BSONType -> SchemaTy -> TypecheckResult BSONType
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
              case m Map.!? k of
                Nothing -> do
                  ty <- withContext (typeOfExpression baseSch v) (PP.text ("Inserting expression " ++ oneLine v ++ " at " ++ show k))
                  return $ Map.insert k ty acc
                Just ogTy ->
                  case v of
                    Inclusion True ->
                      return $ Map.insert k ogTy acc
                    EObject nxtProjExp -> do
                      res <- processInclusions nxtProjExp ogTy baseSch
                      return $ Map.insert k res acc
                    exp -> do
                      ty <- withContext (typeOfExpression baseSch exp) (PP.text ("Getting type of expression: " ++ oneLine exp))
                      return $ Map.insert k ty acc
          )
          Map.empty
          (Map.toList projExp)
    processInclusions _ _ _ = throwErrorWithContext "Invalid projection"
processStage (Match exp) sch = do
  expTy <- typeOfExpression sch exp
  if expTy /= TBool
    then throwErrorWithContext "Match expr of invalid type"
    else case exp of
      Application Eq [FP fp, Lit (Str s)] ->
        narrowDiscUnion fp (== s) sch
      _ -> return sch
processStage (Group groupByExpr accumulations) schema = do
  idType <- typeOfExpression schema groupByExpr
  accTypes <- mapM (\(key, (acc, exp)) -> (,,) key acc <$> typeOfExpression schema exp) (Map.toList accumulations)
  returnTypes <- Map.fromList <$> mapM (\(key, acc, expT) -> (,) key <$> getAccReturnT acc expT) accTypes
  let newSchema = Map.insert "_id" idType returnTypes
  return $ S $ Set.singleton newSchema

getAccReturnT :: Accumulator -> BSONType -> TypecheckResult BSONType
getAccReturnT acc bty = getAccReturnTHelper acc (flattenBSONType bty)

getAccReturnTHelper :: Accumulator -> BSONType -> TypecheckResult BSONType
getAccReturnTHelper AAvg TNumber = return TNumber
getAccReturnTHelper First a = return a
getAccReturnTHelper Last a = return a
getAccReturnTHelper AMin TNumber = return TNumber
getAccReturnTHelper AMax TNumber = return TNumber
getAccReturnTHelper Push t = return $ TArray t
getAccReturnTHelper Sum TNumber = return TNumber
getAccReturnTHelper acc t = throwErrorWithContext $ "Not proper argument type " ++ oneLine t ++ "for accumulator " ++ oneLine acc

typecheck :: AST -> SchemaTy -> TypecheckResult SchemaTy
typecheck (Pipeline []) ty = flattenSchemaTy ty
typecheck (Pipeline (hd : tl)) ty =
  do
    nextSch <- withContext (processStage hd ty) (PP.text ("Typechecking stage: " ++ oneLine hd))
    cleanSch <- flattenSchemaTy nextSch
    typecheck (Pipeline tl) cleanSch
