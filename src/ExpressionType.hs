module ExpressionType (typeOfExpression) where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Schema (accessPossibleTys)
import qualified Text.PrettyPrint as PP
import Types (BSON (..), BSONType (..), Expression (..), FieldPath (..), Index (..), Op (..), SchemaTy (..), TypecheckResult)
import Utils (isSubtype, throwErrorWithContext, withContext)

sumT :: [BSONType] -> BSONType
sumT = TSum . Set.fromList

typeOfOp :: Op -> [BSONType] -> TypecheckResult BSONType
typeOfOp Add [TNumber, TNumber] = return TNumber
typeOfOp Abs [TNumber] = return TNumber
typeOfOp Ceil [TNumber] = return TNumber
typeOfOp Floor [TNumber] = return TNumber
typeOfOp Avg [TArray TNumber] = return TNumber
typeOfOp Max [TArray TNumber] = return TNumber
typeOfOp Min [TArray TNumber] = return TNumber
typeOfOp Eq [t1, t2] =
  if isSubtype t1 t2 || isSubtype t2 t1
    then return TBool
    else throwErrorWithContext ("Equality must check between identical types: " ++ show t1 ++ ", " ++ show t2)
typeOfOp _ _ = throwErrorWithContext "Not acceptable parameters for operation."

-- typeOfOp :: Op -> ([BSONType], BSONType)
-- typeOfOp Add = ([TNumber, TNumber], TNumber)
-- typeOfOp Abs = ([TNumber], TNumber)
-- typeOfOp Ceil = ([TNumber], TNumber)
-- typeOfOp Floor = ([TNumber], TNumber)
-- typeOfOp Avg = ([TArray TNumber], TNumber)
-- typeOfOp Min = ([TArray TNumber], TNumber)
-- typeOfOp Max = ([TArray TNumber], TNumber)

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
typeOfExpression s (FP fp) = TSum . Set.fromList <$> accessPossibleTys fp s
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
