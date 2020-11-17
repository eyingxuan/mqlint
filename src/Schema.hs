module Schema (accessPossibleTys, narrowDiscUnion, updateSchemaTy) where

import Control.Monad (filterM, foldM, mapM)
import Data.Map (Map, insert, (!?))
import Types (BSONType (..), FieldPath, Index (..), SchemaMap, SchemaTy (..))
import Utils (withErr)

updateSchemaTy :: FieldPath -> (BSONType -> Either String BSONType) -> SchemaTy -> Either String SchemaTy
updateSchemaTy [] _ s = return s
updateSchemaTy _ _ (S []) = Left "Schema is empty"
updateSchemaTy (ArrayIndex : _) _ _ = Left "Cannot array index into schema"
updateSchemaTy (ObjectIndex s : tl) trans (S l) =
  S
    <$> mapM
      ( \m -> do
          ty <- withErr (m !? s) "Field name not found in object"
          transTy <- helper tl trans ty
          return $ insert s transTy m
      )
      l
  where
    helper :: FieldPath -> (BSONType -> Either String BSONType) -> BSONType -> Either String BSONType
    helper [] trans ty = trans ty
    helper (ArrayIndex : tl) trans (TArray ty) = do
      newT <- helper tl trans ty
      return $ TArray newT
    helper (ArrayIndex : _) _ _ = Left "Cannot array index into non-array type"
    helper ((ObjectIndex s) : tl) trans (TObject m) = do
      fty <- withErr (m !? s) "Field name not found in object"
      transFty <- helper tl trans fty
      return $ TObject (insert s transFty m)
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

type TypePredicate = Map String BSONType -> Either String Bool

-- Only meaningful to discriminate a sum type
-- Supports one level deep discrimination
narrowDiscUnion :: TypePredicate -> SchemaTy -> Either String SchemaTy
narrowDiscUnion _ (S []) = Left "Schema is empty"
narrowDiscUnion pred (S tyl) = do
  resTys <- filterM pred tyl
  case resTys of
    [] -> Left "Resultant type is empty"
    l -> return $ S l
