module ParserCommon (TransformResult, getStringValue, getValue, JSON (..)) where

import Data.Map (Map)
import qualified Data.Map as Map

-- Structured JSON is the intermediate representation between input text and 
-- a fully parsed pipeline.
data JSON
  = JNumber Double
  | JStr String
  | JObject (Map String JSON)
  | JArray [JSON]
  | JNull
  | JBool Bool
  deriving (Eq, Show)

type TransformResult a = Either String a

getStringValue :: Ord k => k -> Map k JSON -> TransformResult String
getStringValue k m = case getValue k m of
  Right (JStr s) -> Right s
  _ -> Left "could not parse string."

getValue :: Ord k => k -> Map k JSON -> TransformResult JSON
getValue k m = case Map.lookup k m of
  Just v -> Right v
  Nothing -> Left "Could not find key in stage."