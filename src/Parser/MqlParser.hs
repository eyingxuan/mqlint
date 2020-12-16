{-# LANGUAGE FlexibleContexts #-}

module Parser.MqlParser (getPipelineFromFile, parsePipeline) where

import qualified Data.Map as Map
import Parser.JsonParser (parseJson)
import Parser.ParserCommon (JSON (..), TransformResult (..), getStringValue, getValue)
import Text.ParserCombinators.Parsec
import Types (Contextual (..), AST (..), Accumulator (..), BSON (..), Expression (..), FieldPath (..), Index (..), Op (..), Stage (..))
import Utils (throwErrorWithContext)
import qualified Text.PrettyPrint as PP
import Text.Printf (printf)

indexP :: Parser Index
indexP =
  try (many1 digit) *> pure ArrayIndex
    <|> try (ObjectIndex <$> many1 (noneOf "\t\n."))
    <?> "Index must be alphanumeric."

fieldPathP :: Parser FieldPath
fieldPathP = char '$' *> indexP `sepBy` char '.'

makeLiteral :: JSON -> BSON
makeLiteral (JBool b) = Boolean b
makeLiteral (JNumber n) = Number n
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
operatorOf "$min" = Just Min
operatorOf "$max" = Just Max
operatorOf "$eq" = Just Eq
operatorOf "$objectToArray" = Just ObjectToArray
operatorOf "$concatArrays" = Just ConcatArrays
operatorOf "$concat" = Just Concat
operatorOf "$cond" = Just Cond
operatorOf "$indexOfArray" = Just IndexOfArray
operatorOf _ = Nothing

singleton :: a -> [a]
singleton x = [x]



makeExpression :: JSON -> Expression
makeExpression (JBool b) =  Inclusion b
makeExpression (JNumber 0.0) =  Inclusion False
makeExpression (JNumber _) = Inclusion True
makeExpression (JStr s) = case parse fieldPathP "" s of
  Left _ -> Lit (Str s)
  Right fp -> FP fp
makeExpression JNull = Lit Null
makeExpression (JArray arr) = EArray $ map makeExpression arr
-- Operators and flat expression objects are both JSON objects.
-- An object is only an operator iff there is exactly one key AND that key
-- is in the set of operators.
makeExpression (JObject o) = case Map.toList o of
  [("$literal", v)] -> Lit $ makeLiteral v
  [(f, v)] -> case operatorOf f of
    Just op ->
      Application op $ case v of
        (JArray arr) -> map makeExpression arr
        _ -> singleton $ makeExpression v
    Nothing -> EObject $ Map.map makeExpression o
  _ -> EObject $ Map.map makeExpression o

parseStage :: [(String, JSON -> TransformResult Stage)] -> JSON -> Integer -> TransformResult Stage
parseStage m (JObject o) n = case Map.toList o of
  [(k, v)] -> case Map.lookup k (Map.fromList m) of
    Just f -> withContext (f v) (PP.text (printf "Parsing stage #%d: %s." n k))
    Nothing -> throwErrorWithContext $ "unrecognized stage " ++ k
  _ -> throwErrorWithContext (printf "Stage #%d must only have one key." n)
parseStage _ _ n = throwErrorWithContext (printf "Stage #%d must be an object." n)

getFieldPathWithoutDollar (JStr s) = case parse fieldPathP "" ("$" ++ s) of
  Left _ -> throwErrorWithContext "error parsing fieldpath"
  Right x -> return x
getFieldPathWithoutDollar _ = throwErrorWithContext "Fieldpath must be a JSON string."

getFieldPath (JStr s) = case parse fieldPathP "" s of
  Left _ -> throwErrorWithContext "error parsing fieldpath"
  Right x -> return x
getFieldPath _ = throwErrorWithContext "Fieldpath must be a JSON string."

accumulatorOf :: String -> TransformResult Accumulator
accumulatorOf a = case a of
  "$avg" -> return AAvg
  "$first" -> return First
  "$last" -> return Last
  "$min" -> return AMin
  "$max" -> return AMax
  "$push" -> return Push
  "$sum" -> return Sum
  _ -> throwErrorWithContext "unknown accumulator."

getAccumulation :: (String, JSON) -> TransformResult (String, (Accumulator, Expression))
getAccumulation (k, JObject o) = case Map.toList o of
  [(acc, exp)] -> (,) k <$> ((,) <$> accumulatorOf acc <*> pure (makeExpression exp))
  _ -> throwErrorWithContext "Exactly one accumulator per field."
getAccumulation _ = throwErrorWithContext "Accumulator inside $group must be an object."

makeStage :: JSON -> Integer -> TransformResult Stage
makeStage =
  parseStage
    [ ( "$match",
        withObj (\o -> Match <$> (makeExpression <$> getValue "$expr" o))
      ),
      ("$unwind", fmap Unwind . getFieldPath),
      ( "$lookup",
        withObj
          ( \o ->
              Lookup <$> getStringValue "from" o
                <*> (getValue "localField" o >>= getFieldPathWithoutDollar)
                <*> (getValue "foreignField" o >>= getFieldPathWithoutDollar)
                <*> getStringValue "as" o
          )
      ),
      ( "$group",
        withObj
          ( \o ->
              Group <$> (makeExpression <$> getValue "_id" o)
                <*> (Map.fromList <$> mapM getAccumulation (Map.toList (Map.delete "_id" o)))
          )
      ),
      ("$facet", withObj (fmap Facet . mapM makePipeline)),
      -- This only supports "a.b.c" fields, not nested projections.
      ("$project", withObj (pure . Project . Map.map makeExpression))
    ]
  where
    withObj f (JObject o) = f o
    withObj _ _ = throwErrorWithContext "Expecting object."

makePipeline :: JSON -> TransformResult AST
makePipeline (JArray l) = do
  v <- helper l 1 
  return (Pipeline v)
  where
    helper [] _ = return []
    helper (s : ss) n = do
      h <- makeStage s n
      t <- helper ss (n + 1)
      return (h : t)
makePipeline _ = throwErrorWithContext "Pipeline must be array of stages."

parsePipeline :: String -> TransformResult AST
parsePipeline contents = do
  case parseJson contents of
    Right json -> makePipeline json
    Left x -> throwErrorWithContext (show x)

getPipelineFromFile :: String -> IO (TransformResult AST)
getPipelineFromFile filename = do
  contents <- readFile filename
  case parseJson contents of
    Right json -> return $ makePipeline json
    Left s -> return $ throwErrorWithContext (show s)