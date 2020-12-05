module ExpressionType where

import qualified Control.Monad as Monad
import Control.Monad.Except (MonadError (throwError))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Schema (accessPossibleTys)
import Types (BSON (..), BSONType (..), Exception, Expression (..), FieldPath (..), Index (..), Op (..), SchemaTy (..))

sumT :: [BSONType] -> BSONType
sumT = TSum . Set.fromList

typeOfOp :: Op -> [BSONType] -> Exception BSONType
typeOfOp Add [TNumber, TNumber] = return TNumber
typeOfOp Abs [TNumber] = return TNumber
typeOfOp Ceil [TNumber] = return TNumber
typeOfOp Floor [TNumber] = return TNumber
typeOfOp Avg [TArray TNumber] = return TNumber
typeOfOp Max [TArray TNumber] = return TNumber
typeOfOp Min [TArray TNumber] = return TNumber
typeOfOp Eq [t1, t2] = if t1 == t2 then return TBool else throwError "Equality must check between identical types."
typeOfOp _ _ = throwError "Not acceptable parameters for operation."

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

toFieldPath :: [String] -> Exception FieldPath
toFieldPath fs =
  if "\n" `elem` fs
    then throwError "Inclusions cannot index into arrays."
    else return $ map ObjectIndex $ reverse fs

typeOfExpression :: [String] -> SchemaTy -> Expression -> Exception BSONType
typeOfExpression _ s (FP fp) = TSum . Set.fromList <$> accessPossibleTys fp s
typeOfExpression f s (EArray exps) =
  TArray . TSum . Set.fromList <$> mapM (typeOfExpression ("\n" : f) s) exps
typeOfExpression _ _ (Lit bson) = return $ typeFromBson bson
typeOfExpression f s (EObject obj) =
  TObject . Map.fromList
    <$> mapM
      ( \(k, exp) ->
          (,) k <$> typeOfExpression (k : f) s exp
      )
      (Map.toList obj)
typeOfExpression f s (Inclusion i) = do
  fp <- toFieldPath f
  tys <- accessPossibleTys fp s
  if i then return $ TSum $ Set.fromList tys else return $ TSum Set.empty
typeOfExpression f s (Application op args) = undefined

{-

T1 { v: "1" , x : { y: 1 }} | T2 { v: "2", x : { y : 1}}

T1 { x : {y : TInt}} | T2 { x : {y : TStr}}

T1 { x : {y : TInt | TStr}}

-}
