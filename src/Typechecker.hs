-- TODO: Change to not export processStage
module Typechecker (processStage) where

import Control.Monad.Except (ExceptT, MonadError (throwError))
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (MonadReader (ask), ReaderT, lift)
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Schema (accessPossibleTys, updateSchemaTy)
import Types (BSONType (..), Context, SchemaTy (..), Stage (..))
import Utils (withErr)

type TypecheckResult = ReaderT Context (ExceptT String Identity)

-- isSubtype t1 t2 checks if t1 <: t2
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

processStage :: Stage -> SchemaTy -> TypecheckResult SchemaTy
processStage (Unwind fp) sch =
  lift $
    updateSchemaTy
      fp
      ( \ty -> case ty of
          TArray x -> return x
          _ -> throwError "Cannot unwind non-array type"
      )
      sch
processStage (Lookup foreignCol localFp foreignFp as) sch = do
  ctx <- ask
  foreignSch <- lift $ withErr (ctx Map.!? foreignCol) "No such foreign collection"
  localTys <- lift $ accessPossibleTys localFp sch
  foreignTys <- lift $ accessPossibleTys foreignFp foreignSch
  if length localTys == 1 && length foreignTys == 1
    then case (localTys, foreignTys) of
      ([lty], [fty]) ->
        if isSubtype lty fty || isSubtype fty lty
          then
            let (S fsl, S lsl) = (foreignSch, sch)
             in return (S (Set.fromList [Map.insert as (TObject fm) lm | fm <- Set.toList fsl, lm <- Set.toList lsl]))
          else throwError "Local and foreign lookup fields do not match"
      _ -> throwError "not possible"
    else throwError "Local and foreign lookup fields do not match"
processStage _ _ = undefined

-- typecheck :: Context -> BSONType -> AST -> Either String BSONType
-- typecheck ctx expTy ast = undefined