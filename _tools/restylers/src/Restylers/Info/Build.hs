{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Details about how to build a Restyler Docker image
--
-- Module      : Restylers.Info.Build
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Info.Build
  ( RestylerBuild (..)
  , restylerBuild
  , build
  ) where

import Restylers.Prelude

import Data.Aeson
import Restylers.Env
import Restylers.Image
import System.FilePath (takeDirectory, (</>))
import System.Process.Typed

data RestylerBuild = RestylerBuild
  { path :: FilePath
  , dockerfile :: FilePath
  , versionCache :: FilePath
  , options :: [String]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON)

restylerBuild :: FilePath -> RestylerBuild
restylerBuild yaml =
  RestylerBuild
    { path
    , dockerfile = path </> "Dockerfile"
    , versionCache = path </> ".version"
    , options = []
    }
 where
  path = takeDirectory yaml

build
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasLogger env)
  => Bool
  -> RestylerBuild
  -> RestylerImage
  -> m RestylerImage
build quiet RestylerBuild {..} image = do
  env <- getRestylersEnv

  let args =
        concat
          [ ["buildx", "build"]
          , ["--quiet" | quiet]
          , ["--cache-from=type=gha" | env.gha]
          , ["--cache-to=type=gha,mode=max" | env.gha]
          , ["--no-cache" | env.buildNoCache]
          , ["--tag", unImage image]
          , ["--file", dockerfile]
          , [path]
          ]

  p <- loggedProc "docker" args
  image <$ runProcess_ p

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
