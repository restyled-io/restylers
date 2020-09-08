{-# LANGUAGE DerivingVia #-}

module Restylers.Info
    ( RestylerInfo(..)
    )
where

import RIO

import Data.Aeson
import Data.Semigroup (Last(..))
import Data.Semigroup.Generic
import Restylers.Image
import Restylers.Info.Metadata (Metadata)
import Restylers.Name
import Restylers.Version

data RestylerInfo = RestylerInfo
    { enabled :: Maybe (Last Bool)
    , name :: Last RestylerName
    , version :: Maybe (Last RestylerVersion)
    , version_cmd :: Maybe (Last String)
    , image :: Maybe (Last RestylerImage)
    , command :: Maybe (Last [Text])
    , arguments :: Maybe (Last [Text])
    , include :: Maybe (Last [Text])
    , interpreters :: Maybe (Last [Text])
    , supports_arg_sep :: Maybe (Last Bool)
    , supports_multiple_paths :: Maybe (Last Bool)
    , documentation :: Maybe (Last [Text])
    , metadata :: Maybe (Last Metadata)
    }
    deriving stock Generic
    deriving anyclass FromJSON
    deriving Semigroup via (GenericSemigroupMonoid RestylerInfo)
