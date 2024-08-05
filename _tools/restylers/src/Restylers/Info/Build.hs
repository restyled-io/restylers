{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Details about how to build a Restyler Docker image
module Restylers.Info.Build
  ( RestylerBuild (..)
  , restylerBuild
  , build
  ) where

import Restylers.Prelude

import Data.Aeson
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
  :: MonadIO m
  => Bool
  -> RestylerBuild
  -> RestylerImage
  -> m RestylerImage
build quiet RestylerBuild {..} image = image <$ runProcess_ (proc "docker" args)
 where
  args =
    concat
      [ ["buildx", "build"]
      , ["--quiet" | quiet]
      , ["--tag", unImage image]
      , ["--file", dockerfile]
      , [path]
      ]

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
