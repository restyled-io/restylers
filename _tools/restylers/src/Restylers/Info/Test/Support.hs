-- |
--
-- Module      : Restylers.Info.Test.Support
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Info.Test.Support
  ( Support (..)
  )
where

import Restylers.Prelude

import Data.Aeson

data Support = Support
  { path :: FilePath
  , contents :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)
