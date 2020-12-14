module Main where

import Control.Monad (forM_)
import qualified Data.Map as Map
import Lib (PP (..), getContextFromFile, getPipelineFromFile, runTypechecker)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    schemaFile : pipelineFile : collectionName : _ -> do
      ctx <- getContextFromFile schemaFile
      pipe <- getPipelineFromFile pipelineFile
      case (ctx, pipe) of
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
            Nothing -> print (show context ++ "\n---\n" ++ show pipeline)
        (Left cerror, Left perror) -> print (cerror ++ "\n---\n" ++ perror)
        (Left cerror, _) -> print cerror
        (_, Left perror) -> print perror
    _ -> print "pass in a schema file please!"
