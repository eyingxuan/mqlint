module Main where

import qualified Data.Map as Map
import MqlParser (getPipelineFromFile)
import Printing (PP (..))
import SchemaParser (getContextFromFile)
import System.Environment (getArgs)
import Typechecker (runTypechecker)

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
            -- Just schema -> print pipeline
            Just schema -> case runTypechecker pipeline schema context of
              Left error -> print error
              Right (resSchema, warnings) ->
                print (pp resSchema)
            Nothing -> print (show context ++ "\n---\n" ++ show pipeline)
        (Left cerror, Left perror) -> print (cerror ++ "\n---\n" ++ perror)
        (Left cerror, _) -> print cerror
        (_, Left perror) -> print perror
    _ -> print "pass in a schema file please!"
