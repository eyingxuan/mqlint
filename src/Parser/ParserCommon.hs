{-# LANGUAGE FlexibleInstances #-}

module Parser.ParserCommon (TransformResult, getStringValue, getValue, JSON (..), runTransformResult) where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (ReaderT, ask, runReaderT, withReaderT)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint (Doc, nest, ($+$))
import qualified Text.PrettyPrint as PP
import Types (Contextual (..))
import Utils (throwErrorWithContext)

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

type ParserTrace = Doc -> Doc

type TransformResult = ReaderT ParserTrace (ExceptT String Identity)

instance Contextual TransformResult where
  withContext m d = withReaderT (\prevDoc nxtDoc -> prevDoc (d $+$ nest 2 nxtDoc)) m
  getContext = ask

runTransformResult :: TransformResult a -> Either String a
runTransformResult t = runIdentity (runExceptT (runReaderT t id))

getStringValue :: String -> Map String JSON -> TransformResult String
getStringValue k m = do
  val <- getValue k m
  case val of
    JStr s -> return s
    _ -> throwErrorWithContext ("could not parse string value " ++ k)

getValue :: String -> Map String JSON -> TransformResult JSON
getValue k m = withContext helper (PP.text ("Retrieving value of key " ++ k ++ " from object..."))
  where
    helper = case Map.lookup k m of
      Just v -> return v
      Nothing -> throwErrorWithContext ("Could not find key in stage: " ++ k)
