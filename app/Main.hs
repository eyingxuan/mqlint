module Main where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Lib (PP (..), getContextFromFile, getPipelineFromFile, runTransformResult, runTypechecker)
import System.Environment (getArgs)
import Text.Printf(printf)

printFlashy s = putStrLn ("***" ++ s ++ "***\n")

main :: IO ()
main = do
  args <- getArgs
  case args of
    schemaFile : pipelineFile : collectionName : _ -> do
      ctx <- getContextFromFile schemaFile
      pipe <- getPipelineFromFile pipelineFile
      case (runTransformResult ctx, runTransformResult pipe) of
        (Right context, Right pipeline) ->
          case Map.lookup collectionName context of
            Just schema -> case runTypechecker pipeline schema context of
              Left error -> do
                putStrLn "Typechecking failed:"
                putStrLn error
              Right (resSchema, warnings) -> do
                print (pp resSchema)
                putStrLn "\n---\nWarnings:\n"
                forM_ warnings putStrLn
            Nothing -> printf "Given collection name '%s' not found in schema file." collectionName
        (Left cerror, Left perror) -> do
          printFlashy "Parsing errors" 
          putStrLn "in schema:" 
          putStrLn cerror
          putStrLn "---\n"
          putStrLn "in pipeline:"
          putStrLn perror
        (Left cerror, _) -> printFlashy "Schema parse error:" >> putStrLn cerror
        (_, Left perror) -> printFlashy "Pipeline parse error" >> putStrLn perror
    _ -> print "Usage: ./typecheck [schema.json] [pipeline.json] [collectionName]"
