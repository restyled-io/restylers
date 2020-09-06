{-# LANGUAGE NamedFieldPuns #-}

module Restylers.Manifest
    ( Restyler(..)
    , toRestyler
    , write
    )
where

import RIO

import Data.Aeson
import qualified Data.Yaml as Yaml
import Restylers.Image
import qualified Restylers.Info.Resolved as Info
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
    , documentation :: [Text]
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON)

toRestyler :: Info.RestylerInfo -> RestylerImage -> Restyler
toRestyler Info.RestylerInfo { enabled, name, command, arguments, include, interpreters, supports_arg_sep, supports_multiple_paths, documentation } image
    = Restyler
        { enabled
        , name
        , image
        , command
        , arguments
        , include
        , interpreters
        , supports_arg_sep
        , supports_multiple_paths
        , documentation
        }

write :: MonadIO m => FilePath -> NonEmpty Restyler -> m ()
write path = liftIO . Yaml.encodeFile path
