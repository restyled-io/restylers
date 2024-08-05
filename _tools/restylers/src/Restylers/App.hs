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
  { appLogger :: Logger
  , appOptions :: Options
  }

instance HasLogger App where
  loggerL = lens appLogger $ \x y -> x {appLogger = y}

instance HasOptions App where
  optionsL = lens appOptions $ \x y -> x {appOptions = y}

newtype AppT m a = AppT
  { unAppT :: ReaderT App m a
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
  runReaderT (unAppT f) $ App logger opts
