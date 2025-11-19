{-# LANGUAGE NamedFieldPuns #-}

-- |
--
-- Module      : Restylers.Info.Resolved
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Info.Resolved
  ( RestylerInfo (..)
  , ImageSource (..)
  , load
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.Semigroup (Last (..))
import Data.Yaml qualified as Yaml
import Restylers.Image
import Restylers.Info qualified as Info
import Restylers.Info.Build (RestylerBuild, restylerBuild)
import Restylers.Info.Metadata (Metadata)
import Restylers.Info.Metadata qualified as Metadata
import Restylers.Name
import Restylers.Override qualified as Override
import Restylers.Version
import System.FilePath ((<.>), (</>))

data RestylerInfo = RestylerInfo
  { enabled :: Bool
  , name :: RestylerName
  , command :: [Text]
  , arguments :: [Text]
  , include :: [Text]
  , interpreters :: [Text]
  , supports_arg_sep :: Bool
  , supports_multiple_paths :: Bool
  , run_as_filter :: Bool
  , documentation :: [Text]
  , metadata :: Metadata
  , imageSource :: ImageSource
  }

data ImageSource
  = Explicit RestylerImage
  | BuildVersionCmd RestylerName String RestylerBuild
  | BuildVersion RestylerName RestylerVersion RestylerBuild
  deriving stock (Eq, Show)

getImageSource :: MonadIO m => FilePath -> Info.RestylerInfo -> m ImageSource
getImageSource yaml info =
  case ( getLast $ info.name
       , getLast <$> info.version
       , getLast <$> info.version_cmd
       , getLast <$> info.image
       ) of
    (name, Nothing, Nothing, Nothing) ->
      throwString
        $ unpack name.unwrap
        <> ": one of image, version_cmd, or version must be specified"
    (_, Nothing, Nothing, Just image) -> pure $ Explicit image
    (name, Nothing, Just cmd, _) ->
      pure $ BuildVersionCmd name cmd build
    (name, Just version, _, _) ->
      pure $ BuildVersion name version build
 where
  build = fromMaybeLast (restylerBuild yaml) $ info.build

load :: MonadIO m => FilePath -> m RestylerInfo
load yaml = do
  eOverride <- Override.load yaml

  case eOverride of
    Left _ -> do
      info <- decodeYaml yaml
      imageSource <- getImageSource yaml info
      pure $ fromInfo info imageSource
    Right override -> do
      let overridesYaml =
            unpack override.overrides.unwrap
              </> "info"
                <.> "yaml"
      info <- decodeYaml overridesYaml
      imageSource <- getImageSource overridesYaml info
      pure $ fromInfo (info <> overrideToInfo override) imageSource

fromInfo :: Info.RestylerInfo -> ImageSource -> RestylerInfo
fromInfo info imageSource =
  RestylerInfo
    { enabled = fromMaybeLast False $ info.enabled
    , name
    , command = fromMaybeLast [name.unwrap] $ info.command
    , arguments = fromMaybeLast [] $ info.arguments
    , include = fromMaybeLast [] $ info.include
    , interpreters = fromMaybeLast [] $ info.interpreters
    , supports_arg_sep = fromMaybeLast True $ info.supports_arg_sep
    , supports_multiple_paths =
        fromMaybeLast True
          $ info.supports_multiple_paths
    , run_as_filter =
        fromMaybeLast False
          $ info.run_as_filter
    , documentation = fromMaybeLast [] $ info.documentation
    , metadata = fromMaybeLast Metadata.emptyMetadata $ info.metadata
    , imageSource
    }
 where
  name = getLast $ info.name

overrideToInfo :: Override.RestylerOverride -> Info.RestylerInfo
overrideToInfo
  Override.RestylerOverride
    { enabled
    , name
    , command
    , arguments
    , include
    , interpreters
    , supports_arg_sep
    , supports_multiple_paths
    , run_as_filter
    , documentation
    , metadata
    } =
    Info.RestylerInfo
      { enabled
      , name
      , Info.version = Nothing
      , Info.version_cmd = Nothing
      , Info.image = Nothing
      , build = Nothing
      , command
      , arguments
      , include
      , interpreters
      , supports_arg_sep
      , supports_multiple_paths
      , run_as_filter
      , documentation
      , metadata
      }

decodeYaml :: (MonadIO m, FromJSON a) => FilePath -> m a
decodeYaml = liftIO . Yaml.decodeFileThrow

fromMaybeLast :: a -> Maybe (Last a) -> a
fromMaybeLast def = getLast . fromMaybe (Last def)
