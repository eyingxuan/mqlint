module Typechecker () where

import qualified Data.Map.Internal as Map
import Schema (accessPossibleTys)
import Types (AST, BSONType (..), SchemaTy (..), Stage (..))
import Utils (withErr)

type Context = [(String, SchemaTy)]

processStage :: Context -> Stage -> SchemaTy -> Either String SchemaTy
processStage _ (Unwind fp) sch = do
  tys <- accessPossibleTys fp sch
  Left "hello"

-- processStage :: Context -> Stage -> SchemaTy -> Either String SchemaTy
-- processStage _ (Unwind fp) sch = do
--   fty <- withErr (sch Map.!? fp) "Cannot find field"
--   case fty of
--     TArray aty -> return (Map.insert fp aty sch)
--     _ -> Left "Cannot unwind non-array type"
-- processStage ctx (Lookup foreignCol localFp foreignFp as) sch = do
--   foreignSch <- withErr (lookup foreignCol ctx) "No such foreign collection"
--   localTy <- withErr (sch Map.!? localFp) "Local collection does not have specified field"
--   foreignTy <- withErr (foreignSch Map.!? foreignFp) "Foreign collection does not have specified field"
--   if localTy /= foreignTy
--     then Left "Foreign field and local field type mismatch"
--     else
--       let objTy = TObject (Map.toList foreignSch)
--        in return $ Map.insert as objTy sch

-- typecheck :: Context -> BSONType -> AST -> Either String BSONType
-- typecheck ctx expTy ast = undefined