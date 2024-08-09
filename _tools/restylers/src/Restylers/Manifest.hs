{-# LANGUAGE NamedFieldPuns #-}

-- |
--
-- Module      : Restylers.Manifest
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Manifest
  ( Restyler (..)
  , toRestyler
  , write
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.Yaml qualified as Yaml
import Restylers.Image
import Restylers.Info.Metadata (Metadata)
import Restylers.Info.Resolved qualified as Info
import Restylers.Name

data Restyler = Restyler
  { enabled :: Bool
  , name :: RestylerName
  , image :: RestylerImage
  , command :: [Text]
  , arguments :: [Text]
  , include :: [Text]
  , interpreters :: [Text]
  , supports_arg_sep :: Bool
  , supports_multiple_paths :: Bool
  , run_as_filter :: Bool
  , documentation :: [Text]
  , metadata :: Metadata
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

toRestyler :: Info.RestylerInfo -> RestylerImage -> Restyler
toRestyler
  Info.RestylerInfo
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
    }
  image =
    Restyler
      { enabled
      , name
      , image
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

write :: MonadIO m => FilePath -> NonEmpty Restyler -> m ()
write path = liftIO . Yaml.encodeFile path
