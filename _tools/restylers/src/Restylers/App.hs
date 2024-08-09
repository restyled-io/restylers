-- |
--
-- Module      : Restylers.App
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
{-# LANGUAGE DerivingVia #-}

module Restylers.App
  ( App
  , AppT
  , runAppT
  )
where

import Restylers.Prelude

import Blammo.Logging.Simple
import Restylers.Options

data App = App
  { logger :: Logger
  , options :: Options
  }

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasOptions App where
  optionsL = lens (.options) $ \x y -> x {options = y}

newtype AppT m a = AppT
  { unwrap :: ReaderT App m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader App
    )
  deriving (MonadLogger) via (WithLogger App m)

runAppT :: MonadUnliftIO m => Options -> AppT m a -> m a
runAppT opts f = withLoggerEnv $ \logger -> do
  runReaderT f.unwrap $ App logger opts
