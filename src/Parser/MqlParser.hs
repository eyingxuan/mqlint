{-# LANGUAGE FlexibleContexts #-}

module Parser.MqlParser (getPipelineFromFile, parsePipeline) where

import qualified Data.Map as Map
import Parser.JsonParser (parseJson)
import Parser.ParserCommon (JSON (..), TransformResult, getStringValue, getValue)
import Text.ParserCombinators.Parsec
import Types (AST (..), Accumulator (..), BSON (..), Expression (..), FieldPath (..), Index (..), Op (..), Stage (..))
import Utils (throwErrorWithContext)

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

makeExpression :: JSON -> TransformResult Expression
makeExpression (JBool b) = return $ Inclusion b
makeExpression (JNumber 0.0) = return $ Inclusion False
makeExpression (JNumber _) = return $ Inclusion True
makeExpression (JStr s) = case parse fieldPathP "" s of
  Left _ -> return $ Lit (Str s)
  Right fp -> return $ FP fp
makeExpression JNull = return $ Lit Null
makeExpression (JArray arr) = EArray <$> mapM makeExpression arr
-- Operators and flat expression objects are both JSON objects.
-- An object is only an operator iff there is exactly one key AND that key
-- is in the set of operators.
makeExpression (JObject o) = case Map.toList o of
  [("$literal", v)] -> return $ Lit $ makeLiteral v
  [(f, v)] -> case operatorOf f of
    Just op ->
      Application op <$> case v of
        (JArray arr) -> mapM makeExpression arr
        _ -> singleton <$> makeExpression v
    Nothing -> EObject <$> mapM makeExpression o
  _ -> EObject <$> mapM makeExpression o

parseStage :: [(String, JSON -> TransformResult Stage)] -> JSON -> TransformResult Stage
parseStage m (JObject o) = case Map.toList o of
  [(k, v)] -> case Map.lookup k (Map.fromList m) of
    Just f -> f v
    Nothing -> throwErrorWithContext "unrecognized stage."
  _ -> throwErrorWithContext "Stage must only have one key."
parseStage _ _ = throwErrorWithContext "Stage must be an object."

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
  [(acc, exp)] -> (,) k <$> ((,) <$> accumulatorOf acc <*> makeExpression exp)
  _ -> throwErrorWithContext "Exactly one accumulator per field."
getAccumulation _ = throwErrorWithContext "Accumulator inside $group must be an object."

makeStage :: JSON -> TransformResult Stage
makeStage =
  parseStage
    [ ( "$match",
        withObj
          (\o -> Match <$> (getValue "$expr" o >>= makeExpression))
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
              Group <$> (getValue "_id" o >>= makeExpression)
                <*> (Map.fromList <$> mapM getAccumulation (Map.toList (Map.delete "_id" o)))
          )
      ),
      ("$facet", withObj (fmap Facet . mapM makePipeline)),
      -- This only supports "a.b.c" fields, not nested projections.
      ("$project", withObj (fmap Project . mapM makeExpression))
    ]
  where
    withObj f (JObject o) = f o
    withObj _ _ = throwErrorWithContext "Expecting object."

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