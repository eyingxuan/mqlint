module GoldenTests (goldenTests) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as Map
import Parser.MqlParser (getPipelineFromFile)
import Parser.ParserCommon (runTransformResult)
import Parser.Printing (PP (..))
import Parser.SchemaParser (getContextFromFile)
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import qualified Text.PrettyPrint as PP
import Typechecker.Typechecker (runTypechecker)

goldenTests :: IO ()
goldenTests = defaultMain =<< tests

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive pred (hd : tl) = if pred hd then tl else takeWhileInclusive pred tl

execute :: String -> String -> IO BS.ByteString
execute schemaFile pipelineFile = do
  ctx <- getContextFromFile schemaFile
  pipe <- getPipelineFromFile pipelineFile
  let colName = takeWhileInclusive (== '-') (takeBaseName pipelineFile)
   in case (runTransformResult ctx, runTransformResult pipe) of
        (Right context, Right pipeline) ->
          case Map.lookup colName context of
            -- Just schema -> return $ C.pack $ show (runTypechecker pipeline schema context)
            Just schema -> case runTypechecker pipeline schema context of
              Left err -> return $ C.pack err
              Right (ty, warnings) -> return $ C.pack (PP.render (pp ty) ++ "\n---\nWarnings:\n" ++ concat warnings)
            Nothing -> return $ C.pack "Collection not found"
        (Left cerror, Left perror) -> return $ C.pack $ show cerror ++ "\n---\n" ++ show perror
        (Left cerror, _) -> return $ C.pack $ show cerror
        (_, Left perror) -> return $ C.pack $ show perror

tests :: IO TestTree
tests = do
  pipelines <- findByExtension [".json"] "./examples"
  return $
    testGroup
      "Pipeline query golden tests"
      [ goldenVsString
          (takeBaseName pipelineFile)
          resultFile
          (execute "./examples/schema.json" pipelineFile)
        | pipelineFile <- pipelines,
          takeBaseName pipelineFile /= "schema",
          let resultFile = replaceExtension pipelineFile ".result.txt"
      ]
