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
    , overridingInfo
    )
where

import RIO

import Data.Aeson
import Data.Semigroup (Last)
import Restylers.Info (RestylerInfo(..))
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
    }
    deriving stock Generic
    deriving anyclass FromJSON

overridingInfo :: RestylerOverride -> RestylerInfo
overridingInfo RestylerOverride { enabled, name, command, arguments, include, interpreters, supports_arg_sep, supports_multiple_paths, documentation }
    = RestylerInfo
        { enabled
        , name
        , version = Nothing
        , image = Nothing
        , command
        , arguments
        , include
        , interpreters
        , supports_arg_sep
        , supports_multiple_paths
        , documentation
        }
