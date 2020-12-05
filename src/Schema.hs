module Schema (accessPossibleTys, narrowDiscUnion, updateSchemaTy, insertSchemaPath, removeSchemaPath) where

import Control.Monad (filterM, foldM, mapM)
import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Identity (Identity)
import Data.Map.Internal (Map, delete, empty, insert, (!?))
import qualified Data.Set as Set
import Types (BSONType (..), Exception, FieldPath, Index (..), SchemaMap, SchemaTy (..))
import Utils (withErr)

toBsonType :: SchemaTy -> BSONType
toBsonType (S l) = TSum $ Set.map TObject l

fromBsonType :: BSONType -> Exception SchemaTy
fromBsonType bty = case bty of
  TSum l -> do
    sty <-
      mapM
        ( \ty -> case ty of
            TObject m -> return m
            _ -> throwError "Conversion error"
        )
        (Set.toList l)
    return $ S (Set.fromList sty)
  _ -> throwError "Conversion error"

updateSchemaTy :: FieldPath -> (BSONType -> Exception BSONType) -> SchemaTy -> Exception SchemaTy
updateSchemaTy fp trans sch = do
  sty <- helper fp trans (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> (BSONType -> Exception BSONType) -> BSONType -> Exception BSONType
    helper [] trans ty = trans ty
    helper (ArrayIndex : tl) trans (TArray ty) = do
      newT <- helper tl trans ty
      return $ TArray newT
    helper fp@(ArrayIndex : _) trans (TSum tyl) =
      TSum . Set.fromList
        <$> foldM
          ( \acc ty ->
              case ty of
                TArray aty -> do
                  resTy <- helper fp trans (TArray aty)
                  return $ resTy : acc
                _ -> throwError "Cannot array index into sum type containing non-array type"
          )
          []
          tyl
    helper (ArrayIndex : _) _ _ = throwError "Cannot array index into non-array type"
    helper ((ObjectIndex s) : tl) trans (TObject m) = do
      fty <- withErr (m !? s) "Field name not found in object"
      transFty <- helper tl trans fty
      return $ TObject (insert s transFty m)
    helper fp@(ObjectIndex _ : _) trans (TSum tyl) =
      TSum . Set.fromList
        <$> foldM
          ( \acc ty ->
              case ty of
                TObject oty -> do
                  resTy <- helper fp trans (TObject oty)
                  return $ resTy : acc
                _ -> throwError "Cannot object index into sum type containing non-object type"
          )
          []
          tyl
    helper ((ObjectIndex _) : _) _ _ = throwError "Cannot object index into non-object type"

removeSchemaPath :: FieldPath -> SchemaTy -> Exception SchemaTy
removeSchemaPath fp sch = do
  sty <- helper fp (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> BSONType -> Exception BSONType
    helper [] _ = throwError "Cannot remove with empty field path"
    helper [ObjectIndex s] (TObject m) = return (TObject $ delete s m)
    helper (ObjectIndex s : tl) (TObject m) = case m !? s of
      Just fty -> do
        transTy <- helper tl fty
        return (TObject $ insert s transTy m)
      Nothing -> return $ TObject m
    helper _ _ = throwError "Cannot index object"

-- insertion errors if the type already exists on an object
-- mongo does not allow overlapping projections anyways
-- however, this does limit the use of this function
insertSchemaPath :: FieldPath -> BSONType -> SchemaTy -> Exception SchemaTy
insertSchemaPath fp newTy sch = do
  sty <- helper fp newTy (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> BSONType -> BSONType -> Exception BSONType
    helper [] _ _ = throwError "Cannot insert with empty field path"
    helper [ObjectIndex s] newTy (TObject m) = case m !? s of
      Just _ -> throwError "Type already exists at field path"
      Nothing -> return (TObject $ insert s newTy m)
    helper (ObjectIndex s : tl) newTy (TObject m) = case m !? s of
      Just fty -> do
        transTy <- helper tl newTy fty
        return (TObject $ insert s transTy m)
      Nothing -> do
        transTy <- helper tl newTy (TObject empty)
        return (TObject $ insert s transTy m)
    helper _ _ _ = throwError "Cannot index object"

accessPossibleTys :: FieldPath -> SchemaTy -> Exception [BSONType]
accessPossibleTys [] _ = throwError "No index provided"
accessPossibleTys path (S l)
  | Set.size l == 0 = throwError "Schema is empty"
  | otherwise = helper path (TSum (Set.map TObject l))
  where
    helper :: FieldPath -> BSONType -> Exception [BSONType]
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
    helper _ _ = throwError "Tried to index into non object"

narrowDiscUnion :: FieldPath -> (String -> Bool) -> SchemaTy -> Exception SchemaTy
narrowDiscUnion fp pred sch = do
  sty <- helper fp pred (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> (String -> Bool) -> BSONType -> Exception BSONType
    helper [ObjectIndex s] pred (TSum tyl) =
      TSum . Set.fromList
        <$> foldM
          ( \acc ty -> case ty of
              TObject m -> do
                fty <- withErr (m !? s) "Cannot find field"
                case fty of
                  TConst s' -> if pred s' then return $ ty : acc else return acc
                  _ -> return (ty : acc)
              _ -> throwError "Cannot object index sum type with non-object types"
          )
          []
          tyl
    helper (ArrayIndex : tl) pred (TArray aty) = do
      newTy <- helper tl pred aty
      return $ TArray newTy
    helper fp@(ArrayIndex : _) pred (TSum tyl) = TSum . Set.fromList <$> mapM (helper fp pred) (Set.toList tyl)
    helper (ArrayIndex : _) _ _ = throwError "Cannot array index into non-array type"
    helper (ObjectIndex s : tl) pred (TObject m) = do
      fty <- withErr (m !? s) "Cannot find field"
      newFty <- helper tl pred fty
      return $ TObject (insert s newFty m)
    helper fp@(ObjectIndex _ : _) pred (TSum tyl) = TSum . Set.fromList <$> mapM (helper fp pred) (Set.toList tyl)
    helper (ObjectIndex _ : _) _ _ = throwError "Cannot object index into non-object type"
    helper [] _ ty = return ty
