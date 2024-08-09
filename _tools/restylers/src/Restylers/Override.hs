-- |
--
-- Module      : Restylers.Override
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Override
  ( RestylerOverride (..)
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.Semigroup (Last)
import Restylers.Info.Metadata (Metadata)
import Restylers.Name

data RestylerOverride = RestylerOverride
  { overrides :: RestylerName
  , enabled :: Maybe (Last Bool)
  , name :: Last RestylerName
  , command :: Maybe (Last [Text])
  , arguments :: Maybe (Last [Text])
  , include :: Maybe (Last [Text])
  , interpreters :: Maybe (Last [Text])
  , supports_arg_sep :: Maybe (Last Bool)
  , supports_multiple_paths :: Maybe (Last Bool)
  , run_as_filter :: Maybe (Last Bool)
  , documentation :: Maybe (Last [Text])
  , metadata :: Maybe (Last Metadata)
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON)
