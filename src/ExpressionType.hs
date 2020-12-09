module ExpressionType (typeOfExpression) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Printing (PP (..), oneLine)
import Schema (accessPossibleTys)
import qualified Text.PrettyPrint as PP
import Types (BSON (..), BSONType (..), Expression (..), FieldPath (..), Index (..), Op (..), SchemaTy (..), TypecheckResult)
import Utils (flattenBSONType, isSubtype, throwErrorWithContext, withContext)

sumT :: [BSONType] -> BSONType
sumT = TSum . Set.fromList

maybeT :: String -> Maybe a -> TypecheckResult a
maybeT err x = case x of
  Just z -> return z
  Nothing -> throwErrorWithContext err

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
typeOfOpHelper Eq [t1, t2] =
  if isSubtype t1 t2 || isSubtype t2 t1
    then return TBool
    else throwErrorWithContext ("Equality must check between identical types: " ++ oneLine t1 ++ ", " ++ oneLine t2)
typeOfOpHelper ArrayToObject [TArray (TObject m)] = do
  kT <- maybeT "Cannot find type of key" (Map.lookup "k" m)
  vT <- maybeT "Cannot find type of value" (Map.lookup "v" m)
  -- TODO: I think this is not expressable in our type system, keys are variable based on input docs
  if kT /= TStr then throwErrorWithContext "Key must be a string" else undefined
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
typeOfOpHelper Convert [TObject m] = do
  from <- maybeT "Cannot get input expression" (Map.lookup "input" m)
  to <- maybeT "Cannot get to expression" (Map.lookup "to" m)
  let onError = Map.lookup "onError" m
  let onNull = Map.lookup "onNull" m
  throwErrorWithContext "Not fully defined"
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

toFieldPath :: [String] -> TypecheckResult FieldPath
toFieldPath fs =
  if "\n" `elem` fs
    then throwErrorWithContext "Inclusions cannot index into arrays."
    else return $ map ObjectIndex $ reverse fs

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
typeOfExpression _ (Inclusion i) = throwErrorWithContext "Inclusion cannot be typed"
typeOfExpression s (Application op args) = do
  argsT <- mapM (typeOfExpression s) args
  typeOfOp op argsT
