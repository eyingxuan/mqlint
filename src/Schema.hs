{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Schema (accessPossibleTys, narrowDiscUnion, updateSchemaTy) where

import Control.Monad (filterM, foldM, mapM)
import Data.Map (Map, insert, (!?))
import Types (BSONType (..), FieldPath, Index (..), SchemaMap, SchemaTy (..))
import Utils (withErr)

toBsonType :: SchemaTy -> BSONType
toBsonType (S l) = TSum $ map TObject l

fromBsonType :: BSONType -> Either String SchemaTy
fromBsonType bty = case bty of
  TSum l -> do
    sty <-
      mapM
        ( \ty -> case ty of
            TObject m -> return m
            _ -> Left "Conversion error"
        )
        l
    return $ S sty
  _ -> Left "Conversion error"

updateSchemaTy :: FieldPath -> (BSONType -> Either String BSONType) -> SchemaTy -> Either String SchemaTy
updateSchemaTy fp trans sch = do
  sty <- helper fp trans (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> (BSONType -> Either String BSONType) -> BSONType -> Either String BSONType
    helper [] trans ty = trans ty
    helper (ArrayIndex : tl) trans (TArray ty) = do
      newT <- helper tl trans ty
      return $ TArray newT
    helper fp@(ArrayIndex : _) trans (TSum tyl) =
      TSum
        <$> foldM
          ( \acc ty ->
              case ty of
                TArray aty -> do
                  resTy <- helper fp trans (TArray aty)
                  return $ resTy : acc
                _ -> Left "Cannot array index into sum type containing non-array type"
          )
          []
          tyl
    helper (ArrayIndex : _) _ _ = Left "Cannot array index into non-array type"
    helper ((ObjectIndex s) : tl) trans (TObject m) = do
      fty <- withErr (m !? s) "Field name not found in object"
      transFty <- helper tl trans fty
      return $ TObject (insert s transFty m)
    helper fp@(ObjectIndex _ : _) trans (TSum tyl) =
      TSum
        <$> foldM
          ( \acc ty ->
              case ty of
                TObject oty -> do
                  resTy <- helper fp trans (TObject oty)
                  return $ resTy : acc
                _ -> Left "Cannot object index into sum type containing non-object type"
          )
          []
          tyl
    helper ((ObjectIndex _) : _) _ _ = Left "Cannot object index into non-object type"

accessPossibleTys :: FieldPath -> SchemaTy -> Either String [BSONType]
accessPossibleTys [] _ = Left "No index provided"
accessPossibleTys _ (S []) = Left "Schema is empty"
accessPossibleTys path (S l) = helper path (TSum (map TObject l))
  where
    helper :: FieldPath -> BSONType -> Either String [BSONType]
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
    helper _ _ = Left "Tried to index into non object"

narrowDiscUnion :: FieldPath -> (String -> Bool) -> SchemaTy -> Either String SchemaTy
narrowDiscUnion fp pred sch = do
  sty <- helper fp pred (toBsonType sch)
  fromBsonType sty
  where
    helper :: FieldPath -> (String -> Bool) -> BSONType -> Either String BSONType
    helper [ObjectIndex s] pred (TSum tyl) =
      TSum
        <$> foldM
          ( \acc ty -> case ty of
              TObject m -> do
                fty <- withErr (m !? s) "Cannot find field"
                case fty of
                  TConst s' -> if pred s' then return $ ty : acc else return acc
                  _ -> return acc
              _ -> Left "Cannot object index sum type with non-object types"
          )
          []
          tyl
    helper (ArrayIndex : tl) pred (TArray aty) = do
      newTy <- helper tl pred aty
      return $ TArray newTy
    helper fp@(ArrayIndex : _) pred (TSum tyl) = TSum <$> mapM (helper fp pred) tyl
    helper (ArrayIndex : _) _ _ = Left "Cannot array index into non-array type"
    helper (ObjectIndex s : tl) pred (TObject m) = do
      fty <- withErr (m !? s) "Cannot find field"
      newFty <- helper tl pred fty
      return $ TObject (insert s newFty m)
    helper fp@(ObjectIndex _ : _) pred (TSum tyl) = TSum <$> mapM (helper fp pred) tyl
    helper (ObjectIndex _ : _) _ _ = Left "Cannot object index into non-object type"
    helper [] _ ty = return ty
