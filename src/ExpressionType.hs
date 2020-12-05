module ExpressionType where

import qualified Control.Monad as Monad
import qualified Data.Set as Set
import Schema (accessPossibleTys)
import Types (BSONType (..), Expression (..), SchemaTy (..))

typeOfExpression :: SchemaTy -> Expression -> Either String BSONType
typeOfExpression s (FP fp) = TSum . Set.fromList <$> accessPossibleTys fp s
typeOfExpression s (EArray exps) = do
  allTypes <- mapM (typeOfExpression s) exps
  let setOfTypes = Set.fromList allTypes
  case Set.toList setOfTypes of
    [t] -> return t
    _ -> Left "Arrays need exactly one type"
typeOfExpression _ _ = undefined
