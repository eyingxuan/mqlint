module JsonParser where

import Numeric (readSigned, readFloat)
import Control.Applicative (empty)

import Data.Map (Map)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec

import ParserCommon (JSON (..))


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