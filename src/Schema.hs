module Schema (accessPossibleTys, narrowDiscUnion, updateSchemaTy, insertSchemaPath, removeSchemaPath) where

import Control.Monad (filterM, foldM, mapM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader (ask))
import Control.Monad.Writer (MonadWriter (tell))
import Data.Map.Internal (Map, delete, empty, insert, member, (!?))
import qualified Data.Set as Set
import Printing (oneLine)
import qualified Text.PrettyPrint as PP
import Types (BSONType (..), FieldPath, Index (..), SchemaMap, SchemaTy (..), TypecheckResult)
import Utils (fromBsonType, throwErrorWithContext, toBsonType, withErr)

updateSchemaTy :: FieldPath -> (BSONType -> TypecheckResult BSONType) -> SchemaTy -> TypecheckResult SchemaTy
updateSchemaTy fp trans sch = do
  sty <- helper fp trans (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> (BSONType -> TypecheckResult BSONType) -> BSONType -> TypecheckResult BSONType
    helper [] trans ty = trans ty
    helper (ArrayIndex : tl) trans (TArray ty) = do
      newT <- helper tl trans ty
      return $ TArray newT
    helper fp@(_ : _) trans (TSum tyl) =
      TSum
        <$> foldM
          ( \acc ty -> do
              resTy <- helper fp trans ty
              return $ Set.insert resTy acc
          )
          Set.empty
          tyl
    helper (ArrayIndex : _) _ _ = throwErrorWithContext "Cannot array index into non-array type"
    helper ((ObjectIndex s) : tl) trans (TObject m) = do
      fty <- withErr (m !? s) "Field name not found in object"
      transFty <- helper tl trans fty
      return $ TObject (insert s transFty m)
    helper ((ObjectIndex _) : _) _ _ = throwErrorWithContext "Cannot object index into non-object type"

removeSchemaPath :: FieldPath -> SchemaTy -> TypecheckResult SchemaTy
removeSchemaPath fp sch = do
  sty <- helper fp (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> BSONType -> TypecheckResult BSONType
    helper [] _ = throwErrorWithContext "Cannot remove with empty field path"
    helper fp@(ObjectIndex _ : _) (TSum s) =
      TSum
        <$> foldM
          ( \acc ty -> do
              resTy <- helper fp ty
              return $ Set.insert resTy acc
          )
          Set.empty
          s
    helper [ObjectIndex s] (TObject m) =
      if member s m
        then return (TObject $ delete s m)
        else do
          (_, d) <- ask
          tell [PP.render $ d (PP.text ("Index " ++ s ++ " does not exist."))]
          return (TObject $ delete s m)
    helper (ObjectIndex s : tl) (TObject m) = case m !? s of
      Just fty -> do
        transTy <- helper tl fty
        return (TObject $ insert s transTy m)
      Nothing -> do
        (_, d) <- ask
        tell [PP.render $ d (PP.text ("Index " ++ s ++ " does not exist."))]
        return $ TObject m
    helper fp ty = throwErrorWithContext ("Cannot index object" ++ show fp ++ ", " ++ show ty)

-- insertion replaces current type at field path
insertSchemaPath :: FieldPath -> BSONType -> SchemaTy -> TypecheckResult SchemaTy
insertSchemaPath fp newTy sch = do
  sty <- helper fp newTy (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> BSONType -> BSONType -> TypecheckResult BSONType
    helper [] _ _ = throwErrorWithContext "Cannot insert with empty field path"
    helper fp@(ObjectIndex _ : _) newTy (TSum s) =
      TSum
        <$> foldM
          ( \acc ty -> do
              resTy <- helper fp newTy ty
              return $ Set.insert resTy acc
          )
          Set.empty
          s
    helper [ObjectIndex s] newTy (TObject m) =
      return (TObject $ insert s newTy m)
    helper (ObjectIndex s : tl) newTy (TObject m) = case m !? s of
      Just fty -> do
        transTy <- helper tl newTy fty
        return (TObject $ insert s transTy m)
      Nothing -> do
        transTy <- helper tl newTy (TObject empty)
        return (TObject $ insert s transTy m)
    helper _ _ _ = throwErrorWithContext "Cannot index non-object"

accessPossibleTys :: FieldPath -> SchemaTy -> TypecheckResult [BSONType]
accessPossibleTys [] _ = throwErrorWithContext "No index provided"
accessPossibleTys path (S l)
  | Set.size l == 0 = throwErrorWithContext "Schema is empty"
  | otherwise = helper path (TSum (Set.map TObject l))
  where
    helper :: FieldPath -> BSONType -> TypecheckResult [BSONType]
    helper [] ty = return [ty]
    helper path (TSum blist) =
      foldM
        ( \l bsonTy -> do
            res <- helper path bsonTy
            return (l ++ res)
        )
        []
        blist
    helper ((ObjectIndex s) : tl) (TObject m) = do
      fty <- withErr (m !? s) "Field name not found in object"
      helper tl fty
    helper (ArrayIndex : tl) (TArray t) = helper tl t
    helper _ _ = throwErrorWithContext "Tried to index into non object"

narrowDiscUnion :: FieldPath -> (String -> Bool) -> SchemaTy -> TypecheckResult SchemaTy
narrowDiscUnion fp pred sch = do
  sty <- helper fp pred (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> (String -> Bool) -> BSONType -> TypecheckResult BSONType
    helper [ObjectIndex s] pred (TSum tyl) =
      TSum
        <$> foldM
          ( \acc ty -> case ty of
              TObject m -> do
                fty <- withErr (m !? s) "Cannot find field"
                case fty of
                  TConst s' -> if pred s' then return (Set.insert ty acc) else return acc
                  _ -> return $ Set.insert ty acc
              _ -> throwErrorWithContext "Cannot object index sum type with non-object types"
          )
          Set.empty
          tyl
    helper (ArrayIndex : tl) pred (TArray aty) = do
      newTy <- helper tl pred aty
      return $ TArray newTy
    helper fp@(ArrayIndex : _) pred (TSum tyl) = TSum . Set.fromList <$> mapM (helper fp pred) (Set.toList tyl)
    helper (ArrayIndex : _) _ _ = throwErrorWithContext "Cannot array index into non-array type"
    helper (ObjectIndex s : tl) pred (TObject m) = do
      fty <- withErr (m !? s) "Cannot find field"
      newFty <- helper tl pred fty
      return $ TObject (insert s newFty m)
    helper fp@(ObjectIndex _ : _) pred (TSum tyl) = TSum . Set.fromList <$> mapM (helper fp pred) (Set.toList tyl)
    helper (ObjectIndex _ : _) _ _ = throwErrorWithContext "Cannot object index into non-object type"
    helper [] _ ty = return ty
