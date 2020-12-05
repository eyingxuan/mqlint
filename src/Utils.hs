module Utils (withErr) where

import Control.Monad.Except (MonadError (throwError))
import Types (Exception)

withErr :: Maybe a -> String -> Exception a
withErr (Just x) _ = return x
withErr Nothing msg = throwError msg