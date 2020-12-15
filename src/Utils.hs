{-# LANGUAGE FlexibleContexts #-}

module Utils
  ( withErr,
    throwErrorWithContext,
    addLintError,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Writer (MonadWriter (tell))
import Data.DList (DList, singleton)
import Text.PrettyPrint (Doc, render, text)
import qualified Text.PrettyPrint as PP
import Types (Contextual (..))

withErr :: (Contextual m, MonadError String m) => Maybe a -> String -> m a
withErr (Just x) _ = return x
withErr Nothing msg = throwErrorWithContext msg

throwErrorWithContext :: (Contextual m, MonadError String m) => String -> m r
throwErrorWithContext s = do
  errCtx <- getContext
  throwError (render (errCtx (text s)))

addLintError :: (MonadWriter (DList String) m, Contextual m) => Doc -> m ()
addLintError warning = do
  ctx <- getContext
  tell $ singleton (PP.render $ ctx warning)
