module SchemaParser where

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Types (BSONType (..), SchemaTy (..))
import ParserCommon (JSON (..), TransformResult, getValue, getStringValue)

typeOf :: String -> BSONType
typeOf "string" = TStr
typeOf "boolean" = TBool
typeOf "double" = TDbl
typeOf "integer" = TIntgr
typeOf "date" = TDate
typeOf "id" = TObjectId
typeOf s = TConst s

typeOfProperty :: JSON -> TransformResult BSONType
typeOfProperty (JObject o) = do
                              tval <- getStringValue "type" o
                              if tval == "object" then
                                do
                                  propsObj <- getValue "properties" o
                                  props <- fromProperties propsObj
                                  return $ TObject props
                              else if tval == "array" then 
                                do
                                  itemObj <- getValue "items" o
                                  itemT <- typeOfProperty itemObj
                                  return $ TArray itemT
                              else if tval == "sum" then
                                do
                                  typesObj <- getValue "types" o
                                  case typesObj of
                                    JArray ts -> let types = mapM typeOfProperty ts in
                                      TSum . Set.fromList <$> types
                                    _ -> Left "sum types must be presented in an array."
                              else return $ typeOf tval 

typeOfProperty _ = Left "Individual properties must be represented as objects with a `type` field"


fromProperties :: JSON -> TransformResult (Map String BSONType)
fromProperties (JObject o) = case getValue "properties" o of
                              Right (JObject p) -> mapM typeOfProperty p
                              _ -> Left "Properties must be an object."
fromProperties _ = Left "Properties must be objects."

-- A schema will have a "type" field that will determine how 
makeSchema :: JSON -> TransformResult (Map String BSONType)
makeSchema (JObject o) = case Map.lookup "type" o of
                          Just (JStr "object") -> fromProperties (JObject o)
                          _ -> Left "Top-level schema type must be object."
makeSchema _ = Left "Schema must be an object."


makeSchemaTop :: JSON -> TransformResult SchemaTy
makeSchemaTop (JArray l) =  S . Set.fromList <$> mapM makeSchema l
makeSchemaTop o@(JObject _) = S . Set.singleton <$> makeSchema o
makeSchemaTop _ = Left "Schema must be an object or array of possible schemas."