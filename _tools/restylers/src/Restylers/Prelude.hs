-- |
--
-- Module      : Restylers.Prelude
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Prelude
  ( module X
  , foldMapM
  , loggedProc
  ) where

import Blammo.Logging as X
import Blammo.Logging.Logger as X (HasLogger)
import Control.Applicative as X (Alternative)
import Control.Monad as X (guard, unless, void, when, (<=<))
import Control.Monad.IO.Class as X (MonadIO (..))
import Control.Monad.Reader as X
  ( MonadReader (..)
  , ReaderT (..)
  , asks
  , runReaderT
  )
import Data.Foldable as X (for_, traverse_)
import Data.List.NonEmpty as X (NonEmpty (..))
import Data.Maybe as X (catMaybes, fromMaybe, listToMaybe)
import Data.Text as X (Text, pack, unpack)
import Data.Text.Encoding as X (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error as X (lenientDecode)
import Data.Traversable as X (for)
import GHC.Generics as X (Generic)
import Lens.Micro as X (Lens', lens, (^?))
import Lens.Micro.Extras as X (view)
import Numeric.Natural as X (Natural)
import Text.Shakespeare.Text as X (ToText (..))
import UnliftIO as X (MonadUnliftIO)
import UnliftIO.Exception as X (catch, throwIO, throwString, tryAny)
import Prelude as X

import Blammo.Logging.Logger (flushLogger)
import System.Process.Typed (ProcessConfig, proc)

-- | <https://hackage-content.haskell.org/package/relude-1.2.2.2/docs/src/Relude.Foldable.Fold.html#foldMapM>
foldMapM
  :: forall b m f a. (Monoid b, Monad m, Foldable f) => (a -> m b) -> f a -> m b
foldMapM f xs = foldr step return xs mempty
 where
  step x r z = f x >>= \y -> r $! z `mappend` y
{-# INLINE foldMapM #-}

loggedProc
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => String
  -> [String]
  -> m (ProcessConfig () () ())
loggedProc cmd args = do
  logDebug $ pack (unwords $ "exec" : cmd : args) :# []
  proc cmd args <$ flushLogger
