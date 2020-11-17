{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Schema (accessPossibleTys) where

import Control.Monad (foldM)
import Data.Map (Map, (!?))
import Types (BSONType (..), FieldPath)
import Utils (withErr)

{-
A schema must be defined on every collection used.
Each schema is made up of an array of UnitSchemas
-}

type TypePredicate = Map String BSONType -> Bool

accessPossibleTys :: [(FieldPath, TypePredicate)] -> BSONType -> Either String [BSONType]
accessPossibleTys [] ty = return [ty]
accessPossibleTys path (TSum blist) =
  foldM
    ( \l bsonTy -> do
        res <- accessPossibleTys path bsonTy
        return (l ++ res)
    )
    []
    blist
accessPossibleTys ((fp, tp) : tl) (TObject m) = do
  if tp m
    then do
      fty <- withErr (m !? fp) "Field name not found in object"
      accessPossibleTys tl fty
    else return []
accessPossibleTys _ _ = Left "Tried to index into non object"
