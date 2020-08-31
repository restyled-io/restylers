module Restylers.Info.Metadata
    ( Metadata(..)
    , load
    )
where

import RIO

import Data.Aeson
import qualified Data.Yaml as Yaml
import Restylers.Info.Test

data Metadata = Metadata
    { languages :: [Text]
    , tests :: [Test]
    }
    deriving stock Generic
    deriving anyclass FromJSON

newtype InfoMetadata = InfoMetadata
    { metadata :: Maybe Metadata
    }
    deriving stock Generic
    deriving anyclass FromJSON

load :: MonadIO m => FilePath -> m Metadata
load path = do
    fromMaybe emptyMetadata . metadata <$> Yaml.decodeFileThrow path

emptyMetadata :: Metadata
emptyMetadata = Metadata { languages = [], tests = [] }
