module Typechecker.ExpressionType (typeOfExpression) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser.Printing (PP (..), oneLine)
import qualified Text.PrettyPrint as PP
import Typechecker.Schema (accessPossibleTys)
import Types (BSON (..), BSONType (..), Contextual (..), Expression (..), Op (..), SchemaTy (..), TypecheckResult)
import Utils (addLintError, flattenBSONType, isSubtype, throwErrorWithContext)

sumT :: [BSONType] -> BSONType
sumT = TSum . Set.fromList

typeOfOp :: Op -> [BSONType] -> TypecheckResult BSONType
typeOfOp op tyl = typeOfOpHelper op (map flattenBSONType tyl)

typeOfOpHelper :: Op -> [BSONType] -> TypecheckResult BSONType
typeOfOpHelper Add [TNumber, TNumber] = return TNumber
typeOfOpHelper Abs [TNumber] = return TNumber
typeOfOpHelper Ceil [TNumber] = return TNumber
typeOfOpHelper Floor [TNumber] = return TNumber
typeOfOpHelper Avg [TArray TNumber] = return TNumber
typeOfOpHelper Max [TArray TNumber] = return TNumber
typeOfOpHelper Min [TArray TNumber] = return TNumber
typeOfOpHelper Eq [t1, t2] = do
  Monad.unless (isSubtype t1 t2 || isSubtype t2 t1) $
    addLintError
      (PP.text ("Equality must check between identical types: " ++ oneLine t1 ++ ", " ++ oneLine t2))
  return TBool
typeOfOpHelper ObjectToArray [TObject m] = do
  let types = snd <$> Map.toList m
  return $
    TArray
      ( TObject
          ( Map.fromList
              [ ("k", TStr),
                ("v", sumT types)
              ]
          )
      )
typeOfOpHelper ConcatArrays [TArray (TArray t)] = return $ TArray t
typeOfOpHelper Concat [TArray TStr] = return TStr
typeOfOpHelper Cond [TBool, t1, t2] = return (sumT [t1, t2])
typeOfOpHelper IndexOfArray [TArray t1, t2] =
  if isSubtype t1 t2 || isSubtype t2 t1
    then return TNumber
    else throwErrorWithContext "Cannot search for given element type in given array."
typeOfOpHelper op args = throwErrorWithContext ("Parameters `" ++ show args ++ "` not acceptable for operation " ++ oneLine op)

typeFromBson :: BSON -> BSONType
typeFromBson (Number _) = TNumber
typeFromBson (Str _) = TStr
typeFromBson (Object o) = TObject $ typeFromBson <$> o
typeFromBson (Array arr) =
  TArray $ TSum $ Set.fromList $ map typeFromBson arr
typeFromBson Null = TNull
typeFromBson (Date _) = TDate
typeFromBson (Boolean _) = TBool
typeFromBson (ObjectId _) = TObjectId

typeOfExpression :: SchemaTy -> Expression -> TypecheckResult BSONType
typeOfExpression s (FP fp) =
  withContext
    (TSum . Set.fromList <$> accessPossibleTys fp s)
    (PP.text ("Accessing field path " ++ oneLine (FP fp) ++ " of schema ") PP.$+$ pp s)
typeOfExpression s (EArray exps) =
  TArray . TSum . Set.fromList <$> mapM (typeOfExpression s) exps
typeOfExpression _ (Lit bson) = return $ typeFromBson bson
typeOfExpression s (EObject obj) =
  TObject . Map.fromList
    <$> mapM
      ( \(k, exp) ->
          (,) k <$> typeOfExpression s exp
      )
      (Map.toList obj)
typeOfExpression _ (Inclusion _) = throwErrorWithContext "Inclusion cannot be typed"
typeOfExpression s (Application op args) = do
  argsT <- mapM (typeOfExpression s) args
  typeOfOp op argsT
