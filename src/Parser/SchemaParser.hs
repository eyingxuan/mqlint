module Parser.SchemaParser (getContextFromFile, parseSchemaTy) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Parser.JsonParser (parseJson)
import Parser.ParserCommon (JSON (..), TransformResult, getStringValue, getValue)
import Types (BSONType (..), Context, SchemaTy (..), Contextual (..) )
import Utils (throwErrorWithContext)
import qualified Text.PrettyPrint as PP
import Text.Printf (printf)

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
  case tval of
    "object" -> do
      props <- fromProperties j
      return $ TObject props
    "array" ->
      do
        itemObj <- getValue "items" o
        itemT <- withContext (typeOfProperty itemObj) (PP.text "Getting inner type of array")
        return $ TArray itemT
    "sum" ->
      do
        typesObj <- getValue "types" o
        case typesObj of
          JArray ts ->
            let types = mapM typeOfProperty ts
             in TSum . Set.fromList <$> types
          _ -> throwErrorWithContext "sum types must be presented in an array."
    _ -> return $ typeOf tval
typeOfProperty _ = throwErrorWithContext "Individual properties must be represented as objects with a `type` field"

mapMWithKeyInContext :: (Monad m, Contextual m) => String -> (b -> m a) -> Map String b -> m (Map String a)
mapMWithKeyInContext msg f = sequence . Map.mapWithKey (\k v -> withContext (f v) (PP.text (printf msg k)))

fromProperties :: JSON -> TransformResult (Map String BSONType)
fromProperties (JObject o) = do
  v <- getValue "properties" o
  case v of
    -- JObject p -> mapM typeOfProperty p
    JObject p -> mapMWithKeyInContext "Property with key %s" typeOfProperty p
    _ -> throwErrorWithContext "Properties must be an object."
fromProperties _ = throwErrorWithContext "Properties must be objects."

-- A schema will have a "type" field that will determine how
makeSchema :: JSON -> TransformResult (Map String BSONType)
makeSchema (JObject o) = case Map.lookup "type" o of
  Just (JStr "object") -> fromProperties (JObject o)
  _ -> throwErrorWithContext "Top-level schema type must be object."
makeSchema _ = throwErrorWithContext "Schema must be an object."

makeSchemaTop :: JSON -> TransformResult SchemaTy
makeSchemaTop (JArray l) = S . Set.fromList <$> mapM makeSchema l
makeSchemaTop o@(JObject _) = S . Set.singleton <$> makeSchema o
makeSchemaTop _ = throwErrorWithContext "Schema must be an object or array of possible schemas."

parseSchemaTy :: String -> TransformResult SchemaTy
parseSchemaTy contents = do
  case parseJson contents of
    Right json -> makeSchemaTop json
    Left x -> throwErrorWithContext (show x)

makeContext :: JSON -> TransformResult Context
makeContext (JObject o) = mapM makeSchemaTop o
makeContext _ = throwErrorWithContext "DB Schema file must be a mapping from collection names to schemas."

getContextFromFile :: String -> IO (TransformResult Context)
getContextFromFile filename = do
  contents <- readFile filename
  case parseJson contents of
    Right json -> return $ makeContext json
    Left s -> return $ throwErrorWithContext (show s)