module Utils (withErr, toBsonType, fromBsonType, isSubtype) where

import Control.Monad.Except (MonadError (throwError))
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Types (BSONType (..), Exception, SchemaTy (..))

toBsonType :: SchemaTy -> BSONType
toBsonType (S l) = TSum $ Set.map TObject l

fromBsonType :: BSONType -> Exception SchemaTy
fromBsonType bty = case bty of
  TSum l -> do
    sty <-
      mapM
        ( \ty -> case ty of
            TObject m -> return m
            _ -> throwError "Conversion error"
        )
        (Set.toList l)
    return $ S (Set.fromList sty)
  _ -> throwError "Conversion error"

withErr :: Maybe a -> String -> Exception a
withErr (Just x) _ = return x
withErr Nothing msg = throwError msg

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