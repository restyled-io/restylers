module Restylers.Info.Metadata
  ( Metadata (..)
  , emptyMetadata
  ) where

import Restylers.Prelude

import Data.Aeson
import Restylers.Info.Test

data Metadata = Metadata
  { languages :: [Text]
  , tests :: [Test]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

emptyMetadata :: Metadata
emptyMetadata = Metadata {languages = [], tests = []}
