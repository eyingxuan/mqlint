module Typechecker () where

import qualified Data.Map.Internal as Map
import Schema (accessPossibleTys, updateSchemaTy)
import Types (AST, BSONType (..), SchemaTy (..), Stage (..))
import Utils (withErr)

type Context = [(String, SchemaTy)]

processStage :: Context -> Stage -> SchemaTy -> Either String SchemaTy
processStage _ (Unwind fp) sch =
  updateSchemaTy
    fp
    ( \ty -> case ty of
        TArray x -> Right x
        _ -> Left "Cannot unwind non-array type"
    )
    sch
processStage ctx (Lookup foreignCol localFp foreignFp as) sch = do
  foreignSch <- withErr (lookup foreignCol ctx) "No such foreign collection"
  localTys <- accessPossibleTys localFp sch
  foreignTys <- accessPossibleTys foreignFp foreignSch
  if length localTys == 1 && length foreignTys == 1
    then case (localTys, foreignTys) of
      ([lty], [fty]) ->
        -- TODO: Use subtyping here instead of eq - For ex. TConst "hello" fits with TStr
        -- Consider warning instead?
        if lty == fty
          then
            let (S fsl, S lsl) = (foreignSch, sch)
             in return (S [Map.insert as (TObject fm) lm | fm <- fsl, lm <- lsl])
          else Left "Local and foreign lookup fields do not match"
      _ -> Left "not possible"
    else Left "Local and foreign lookup fields do not match"

-- typecheck :: Context -> BSONType -> AST -> Either String BSONType
-- typecheck ctx expTy ast = undefined