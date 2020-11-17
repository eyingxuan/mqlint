module Typechecker () where

import qualified Data.Map as Map
import Types (AST, BSONType (..), Stage (..))

type Schema = Map.Map String BSONType

type Context = [(String, Schema)]

withErr :: Maybe a -> String -> Either String a
withErr (Just x) _ = Right x
withErr Nothing msg = Left msg

processStage :: Context -> Stage -> Schema -> Either String Schema
processStage _ (Unwind fp) sch = do
  fty <- withErr (sch Map.!? fp) "Cannot find field"
  case fty of
    TArray aty -> return (Map.insert fp aty sch)
    _ -> Left "Cannot unwind non-array type"
processStage ctx (Lookup foreignCol localFp foreignFp as) sch = do
  foreignSch <- withErr (lookup foreignCol ctx) "No such foreign collection"
  localTy <- withErr (sch Map.!? localFp) "Local collection does not have specified field"
  foreignTy <- withErr (foreignSch Map.!? foreignFp) "Foreign collection does not have specified field"
  if localTy /= foreignTy
    then Left "Foreign field and local field type mismatch"
    else
      let objTy = TObject (Map.toList foreignSch)
       in return $ Map.insert as objTy sch

typecheck :: Context -> BSONType -> AST -> Either String BSONType
typecheck ctx expTy ast = undefined