module Utils (withErr, toBsonType, fromBsonType) where

import Control.Monad.Except (MonadError (throwError))
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