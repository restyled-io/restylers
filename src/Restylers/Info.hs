{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A Faithful representation of the @info.yaml@ format
--
-- All merging and defaulting happens /with/ a value of this type, not (e.g.)
-- within its parsing mechanisms. See "Restylers.Restyler".
--
-- Use of 'Last' means that '(<>)' can be used to combined things, useful with
-- fields of an 'Override' value.
--
module Restylers.Info
    ( RestylerInfo(..)
    , restylerInfoYaml
    , restylerMetadata
    , load
    )
where

import RIO

import Data.Aeson
import Data.Semigroup (Last(..))
import Data.Semigroup.Generic
import qualified Data.Yaml as Yaml
import Restylers.Image
import Restylers.Info.Metadata (Metadata, emptyMetadata)
import Restylers.Name
import Restylers.Version
import RIO.FilePath ((<.>), (</>))
import RIO.Text (unpack)

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

load
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => FilePath
    -> m RestylerInfo
load path = do
    logDebug $ "Reading " <> fromString path
    liftIO $ Yaml.decodeFileThrow path

restylerInfoYaml :: RestylerName -> FilePath
restylerInfoYaml name =
    "restylers" </> unpack (unRestylerName name) </> "info" <.> "yaml"

restylerMetadata :: RestylerInfo -> Metadata
restylerMetadata RestylerInfo { metadata } =
    maybe emptyMetadata getLast metadata
