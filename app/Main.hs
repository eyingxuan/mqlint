module Main where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Lib (PP (..), getContextFromFile, getPipelineFromFile, runTransformResult, runTypechecker)
import System.Environment (getArgs)
import Text.Printf(printf)

errmsg = putStrLn "Error found!"

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
                putStrLn "Error found!"
                putStrLn error
              Right (resSchema, warnings) -> do
                print (pp resSchema)
                putStrLn "\n---\nWarnings:\n"
                forM_ warnings putStrLn
            Nothing -> printf "Given collection name '%s' not found in schema file." collectionName
        (Left cerror, Left perror) -> putStrLn "Errors found! Schema: \n" >> putStrLn (cerror ++ "\n---\nPipeline:\n" ++ perror)
        (Left cerror, _) -> errmsg >> putStrLn cerror
        (_, Left perror) -> errmsg >> putStrLn perror
    _ -> print "Usage: ./typecheck [schema.json] [pipeline.json] [collectionName]"
