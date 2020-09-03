module Restylers.Info.Metadata
    ( Metadata(..)
    , emptyMetadata
    )
where

import RIO

import Data.Aeson
import Restylers.Info.Test

data Metadata = Metadata
    { languages :: [Text]
    , tests :: [Test]
    }
    deriving stock Generic
    deriving anyclass FromJSON

emptyMetadata :: Metadata
emptyMetadata = Metadata { languages = [], tests = [] }
