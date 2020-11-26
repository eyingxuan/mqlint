module Parser where
import Text.ParserCombinators.Parsec
import Types (AST (..), Accumulator (..), BSON (..), Expression (..), Op (..), Stage (..), FieldPath (..), Index (..))
import Control.Applicative ((<$>), (<*>), (<*), (*>), (<$), empty)

import Data.Map (Map)
import Numeric (readSigned, readFloat)
import qualified Data.Map as Map

type TransformResult a = Either String a

indexP :: Parser Index
indexP = try (many1 digit) *> pure ArrayIndex
         <|> try (ObjectIndex <$> many1 (noneOf "\t\n."))
         <?> "Index must be alphanumeric."

fieldPathP :: Parser FieldPath
fieldPathP = char '$' *> indexP `sepBy` char '.'

makeLiteral :: JSON -> BSON
makeLiteral (JBool b) = Boolean b
makeLiteral (JNumber n) = Dbl n
makeLiteral (JStr s) = Str s
makeLiteral JNull = Null
makeLiteral (JObject o) = Object $ makeLiteral <$> o
makeLiteral (JArray arr) = Array $ makeLiteral <$> arr

-- TODO: Add more operators!
operatorOf :: String -> Maybe Op
operatorOf "$add" = Just Add
operatorOf "$abs" = Just Abs
operatorOf "$ceil" = Just Ceil
operatorOf "$floor" = Just Floor
operatorOf "$avg" = Just Avg
operatorOf _ = Nothing


singleton :: a -> [a]
singleton x = [x]

makeExpression :: JSON -> TransformResult Expression
makeExpression (JBool b) = return $ Inclusion b
makeExpression (JNumber 0.0) = return $ Inclusion False
makeExpression (JNumber _) = return $ Inclusion True
makeExpression (JStr s) = case parse fieldPathP "" s of
  Left _ -> return $ Lit (Str s)
  Right fp -> return $ FP fp
makeExpression JNull = return $ Lit Null
makeExpression (JArray arr) = EArray <$> mapM makeExpression arr
makeExpression (JObject o) = case Map.toList o of
  [("$literal", v)] -> return $ Lit $ makeLiteral v
  [(f, v)] -> case operatorOf f of
    Just op -> Application op <$> case v of
      (JArray arr) -> mapM makeExpression arr
      _ -> singleton <$> makeExpression v
    Nothing -> EObject <$> mapM makeExpression o
  -- Operator expressions have exactly one key, otherwise this is an object.
  _ -> EObject <$> mapM makeExpression o 
  
  
parseStage :: [(String, JSON -> TransformResult Stage)] -> JSON -> TransformResult Stage
parseStage m (JObject o) = case Map.toList o of
  [(k, v)] -> case Map.lookup k (Map.fromList m) of
    Just f -> f v
    Nothing -> Left "unrecognized stage."
  _ -> Left "Stage must only have one key."
parseStage _ _ = Left "Stage must be an object."

getStringValue k m = case getValue k m of
  Right (JStr s) -> Right s
  _ -> Left "could not parse string."

getValue k m = case Map.lookup k m of
  Just v -> Right v
  Nothing -> Left "Could not find key in stage."

getFieldPath (JStr s) = case parse fieldPathP "" s of
  Left _ -> Left "error parsing fieldpath"
  Right x -> Right x
getFieldPath _ = Left "Fieldpath must be a JSON string."

accumulatorOf :: String -> Either String Accumulator
accumulatorOf a = case a of
  "$avg" -> Right AAvg
  "$first" -> Right First
  "$last" -> Right Last
  "$min" -> Right AMin
  "$max" -> Right AMax
  _ -> Left "unknown accumulator."

getAccumulation :: (String, JSON) -> TransformResult (String, Accumulator, Expression)
getAccumulation (k, JObject o) = case Map.toList o of
  [(acc, exp)] -> (,,) <$> pure k <*> accumulatorOf acc <*> makeExpression exp
  _ -> Left "Exactly one accumulator per field."

getAccumulation _ = Left "Accumulator inside $group must be an object."


makeStage :: JSON -> TransformResult Stage
makeStage = parseStage [
      ("$match", fmap Match . makeExpression)
    , ("$unwind", fmap Unwind . getFieldPath)
    , ("$lookup", withObj (\o -> Lookup <$> getStringValue "from" o
                                        <*> (getValue "localField" o >>= getFieldPath)
                                        <*> (getValue "foreignField" o >>= getFieldPath)
                                        <*> getStringValue "as" o))
    , ("$group", withObj (\o -> Group <$> (getValue "_id" o >>= makeExpression)
                                      <*> mapM getAccumulation (Map.toList (Map.delete "_id" o))))
    , ("$facet", withObj (fmap Facet . mapM makePipeline))
    -- This only supports "a.b.c" fields, not nested projections.
    , ("$project", withObj (fmap Project . mapM makeExpression))
  ]
  where
    withObj f (JObject o) = f o
    withObj _ _ = Left "Expecting object."

makePipeline :: JSON -> TransformResult AST
makePipeline (JArray l) = do
  v <- helper l
  return (Pipeline v)
  where
    helper [] = return []
    helper (s : ss) = do 
      h <- makeStage s
      t <- helper ss
      return (h : t)

makePipeline _ = Left "Pipeline must be array of stages."

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

jTopP :: Parser JSON
jTopP = whitespace *>
        (JObject <$> jObjP 
        <|> JArray <$> jArrP
        <?> "Top-level must be object or array.")

jArrP :: Parser [JSON]
jArrP = lexeme (char '[') *> jValP `sepBy` lexeme (char ',') <* lexeme (char ']')

jObjP :: Parser (Map String JSON)
jObjP = lexeme (char '{') *> (Map.fromList <$> lexeme jPairP `sepBy` lexeme (char ',')) <* lexeme (char '}')

jPairP :: Parser (String, JSON)
jPairP = (,) <$> lexeme stringV <*> (lexeme (char ':') *> lexeme jValP)

constP :: String -> a -> Parser a
constP s x = string s *> pure x

jValP :: Parser JSON 
jValP = JStr <$> stringV
        <|> JNumber <$> numberV
        <|> JBool <$> (constP "true" True <|> constP "false" False)
        <|> constP "null" JNull
        <|> JObject <$> jObjP
        <|> JArray <$> jArrP
        <?> "JSON value"

whitespace :: Parser ()
whitespace = many (oneOf " \n\t") *> pure ()

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

stringV :: Parser String
stringV = char '\"' *> many (noneOf "\"") <* char '\"'

numberV :: Parser Double
numberV = do s <- getInput
             case readSigned readFloat s of
                [(n, s')] -> n <$ setInput s'
                _         -> empty
