module Typechecker () where

import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Schema (accessPossibleTys, updateSchemaTy)
import Types (AST, BSONType (..), SchemaTy (..), Stage (..))
import Utils (withErr)

type Context = [(String, SchemaTy)]

isSubtype :: BSONType -> BSONType -> Bool
isSubtype (TConst _) TStr = True
isSubtype (TArray ty1) (TArray ty2) = isSubtype ty1 ty2
isSubtype (TSum s1) (TSum s2) =
  all
    ( \ty ->
        any (`isSubtype` ty) s1
    )
    s2
isSubtype (TObject m1) (TObject m2) =
  Map.foldrWithKey
    ( \k v acc ->
        case m1 Map.!? k of
          Just v' -> isSubtype v' v && acc
          Nothing -> False
    )
    True
    m2
isSubtype t1 t2
  | t1 == t2 = True
  | otherwise = False

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
        if isSubtype lty fty || isSubtype fty lty
          then
            let (S fsl, S lsl) = (foreignSch, sch)
             in return (S (Set.fromList [Map.insert as (TObject fm) lm | fm <- Set.toList fsl, lm <- Set.toList lsl]))
          else Left "Local and foreign lookup fields do not match"
      _ -> Left "not possible"
    else Left "Local and foreign lookup fields do not match"

-- typecheck :: Context -> BSONType -> AST -> Either String BSONType
-- typecheck ctx expTy ast = undefined