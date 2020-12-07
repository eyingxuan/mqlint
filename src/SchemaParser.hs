module SchemaParser (getContextFromFile) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import JsonParser (parseJson)
import ParserCommon (JSON (..), TransformResult, getStringValue, getValue)
import Types (BSONType (..), Context, SchemaTy (..))

typeOf :: String -> BSONType
typeOf "string" = TStr
typeOf "boolean" = TBool
typeOf "number" = TNumber
typeOf "date" = TDate
typeOf "id" = TObjectId
typeOf s = TConst s

typeOfProperty :: JSON -> TransformResult BSONType
typeOfProperty j@(JObject o) = do
  tval <- getStringValue "type" o
  if tval == "object"
    then do
      props <- fromProperties j
      return $ TObject props
    else
      if tval == "array"
        then do
          itemObj <- getValue "items" o
          itemT <- typeOfProperty itemObj
          return $ TArray itemT
        else
          if tval == "sum"
            then do
              typesObj <- getValue "types" o
              case typesObj of
                JArray ts ->
                  let types = mapM typeOfProperty ts
                   in TSum . Set.fromList <$> types
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
makeSchemaTop (JArray l) = S . Set.fromList <$> mapM makeSchema l
makeSchemaTop o@(JObject _) = S . Set.singleton <$> makeSchema o
makeSchemaTop _ = Left "Schema must be an object or array of possible schemas."

makeContext :: JSON -> TransformResult Context
makeContext (JObject o) = mapM makeSchemaTop o
makeContext _ = Left "DB Schema file must be a mapping from collection names to schemas."

getContextFromFile :: String -> IO (TransformResult Context)
getContextFromFile filename = do
  contents <- readFile filename
  case parseJson contents of
    Right json -> return $ makeContext json
    Left s -> return $ Left (show s)