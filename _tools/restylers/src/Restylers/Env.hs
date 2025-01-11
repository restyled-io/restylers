-- |
--
-- Module      : Restylers.Env
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Env
  ( RestylersEnv (..)
  , getRestylersEnv
  ) where

import Restylers.Prelude

import System.Environment (lookupEnv)

data RestylersEnv = RestylersEnv
  { testShow :: Bool
  , buildNoCache :: Bool
  , gha :: Bool
  }

getRestylersEnv :: MonadIO m => m RestylersEnv
getRestylersEnv =
  RestylersEnv
    <$> envNonEmpty "RESTYLERS_TEST_SHOW"
    <*> envNonEmpty "RESTYLERS_BUILD_NO_CACHE"
    <*> envSatisfies "GITHUB_ACTIONS" (== "true")

envSatisfies :: MonadIO m => String -> (String -> Bool) -> m Bool
envSatisfies name p = liftIO $ maybe False p <$> lookupEnv name

envNonEmpty :: MonadIO m => String -> m Bool
envNonEmpty name = envSatisfies name $ not . null
