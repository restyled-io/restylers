module Restylers.Info
    ( RestylerInfo
    , load
    , name
    )
where

import RIO

import Data.Aeson
import qualified Data.Yaml as Yaml
import Restylers.Name

newtype RestylerInfo = RestylerInfo
    { name :: RestylerName
    }
    deriving stock Generic
    deriving anyclass FromJSON

load :: MonadIO m => FilePath -> m RestylerInfo
load = Yaml.decodeFileThrow
