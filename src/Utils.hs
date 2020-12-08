module Utils
  ( withErr,
    withContext,
    toBsonType,
    fromBsonType,
    isSubtype,
    flattenSchemaTy,
    throwErrorWithContext,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (MonadReader (ask), withReaderT)
import Data.Bifunctor (second)
import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import Text.PrettyPrint (Doc, nest, render, text, ($+$))
import Types (BSONType (..), SchemaMap (..), SchemaTy (..), TypecheckResult)

toBsonType :: SchemaTy -> BSONType
toBsonType (S l) = TSum $ Set.map TObject l

fromBsonType :: BSONType -> TypecheckResult SchemaTy
fromBsonType bty = case bty of
  TSum l -> do
    sty <-
      mapM
        ( \ty -> case ty of
            TObject m -> return m
            _ -> throwErrorWithContext "Conversion error"
        )
        (Set.toList l)
    return $ S (Set.fromList sty)
  _ -> throwErrorWithContext "Conversion error"

withErr :: Maybe a -> String -> TypecheckResult a
withErr (Just x) _ = return x
withErr Nothing msg = throwErrorWithContext msg

withContext :: TypecheckResult a -> Doc -> TypecheckResult a
withContext m d = withReaderT (second (\prevDoc nxtDoc -> prevDoc d $+$ nest 2 nxtDoc)) m

throwErrorWithContext :: String -> TypecheckResult a
throwErrorWithContext s = do
  (_, errorCtx) <- ask
  throwError (render (errorCtx (text s)))

isEmptySumType :: BSONType -> Bool
isEmptySumType (TSum x) = Set.size x == 0
isEmptySumType _ = False

flattenBSONType :: BSONType -> BSONType
flattenBSONType (TSum x) =
  let newTs = map flattenBSONType (Set.toList x)
   in case filter (not . isEmptySumType) newTs of
        [t] -> t
        _ -> TSum x
flattenBSONType (TObject m) =
  TObject $
    Map.foldrWithKey
      ( \k v acc ->
          let newV = flattenBSONType v
           in if isEmptySumType newV then acc else Map.insert k newV acc
      )
      Map.empty
      m
flattenBSONType (TArray t) = TArray $ flattenBSONType t
flattenBSONType t = t

flattenSchemaMap :: SchemaMap -> TypecheckResult SchemaMap
flattenSchemaMap sch = case flattenBSONType (TObject sch) of
  TObject newSch -> return newSch
  _ -> throwErrorWithContext "Something went wrong"

flattenSchemaTy :: SchemaTy -> TypecheckResult SchemaTy
flattenSchemaTy (S sch) = S . Set.fromList <$> mapM flattenSchemaMap (Set.toList sch)

isSubtype :: BSONType -> BSONType -> Bool
isSubtype t1 t2 =
  isSubtypeHelper (flattenBSONType t1) (flattenBSONType t2)

-- isSubtype t1 t2 checks if t1 <: t2
isSubtypeHelper :: BSONType -> BSONType -> Bool
isSubtypeHelper (TConst _) TStr = True
isSubtypeHelper (TSum t) TStr =
  all
    ( \t -> case t of
        TConst _ -> True
        _ -> False
    )
    t
isSubtypeHelper (TArray ty1) (TArray ty2) = isSubtype ty1 ty2
isSubtypeHelper (TSum s1) (TSum s2) =
  all
    ( \ty ->
        all (`isSubtypeHelper` ty) s2
    )
    s1
isSubtypeHelper (TObject m1) (TObject m2) =
  Map.foldrWithKey
    ( \k v acc ->
        case m1 Map.!? k of
          Just v' -> isSubtypeHelper v' v && acc
          Nothing -> False
    )
    True
    m2
isSubtypeHelper t1 t2
  | t1 == t2 = True
  | otherwise = False