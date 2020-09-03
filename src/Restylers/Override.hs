{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A Faithful representation of the @info.yaml@ format, for an /override/
--
-- All merging and defaulting happens /with/ a value of this type, not (e.g.)
-- within its parsing mechanisms. See "Restylers.Restyler".
--
-- Use of 'Last' means that '(<>)' can be used to combined things, useful with
-- fields of an 'Info' value.
--
module Restylers.Override
    ( RestylerOverride(..)
    , load
    , loadInfo
    , toInfo
    )
where

import RIO

import Control.Error.Util (hush)
import Data.Aeson
import Data.Semigroup (Last)
import qualified Data.Yaml as Yaml
import Restylers.Info (RestylerInfo(..), restylerInfoYaml)
import qualified Restylers.Info as Info
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
    , documentation :: Maybe (Last [Text])
    , metadata :: Maybe (Last Metadata)
    }
    deriving stock Generic
    deriving anyclass FromJSON

load
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> m (Maybe RestylerOverride)
load path = do
    logDebug $ "Reading " <> fromString path <> " (as override)"
    liftIO $ hush <$> Yaml.decodeFileEither path

loadInfo
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => RestylerOverride
    -> m RestylerInfo
loadInfo RestylerOverride { overrides } =
    Info.load $ restylerInfoYaml overrides

toInfo :: RestylerOverride -> RestylerInfo
toInfo RestylerOverride { enabled, name, command, arguments, include, interpreters, supports_arg_sep, supports_multiple_paths, documentation, metadata }
    = RestylerInfo
        { enabled
        , name
        , version = Nothing
        , version_cmd = Nothing
        , image = Nothing
        , command
        , arguments
        , include
        , interpreters
        , supports_arg_sep
        , supports_multiple_paths
        , documentation
        , metadata
        }
