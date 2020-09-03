{-# LANGUAGE NamedFieldPuns #-}

-- | A fully resolved Restyler, as you would find in @restylers.yaml@
module Restylers.Restyler
    ( Restyler(..)
    , loadRestylerInfo
    , mkDevImage
    , mkRestyler
    )
where

import RIO

import Data.Aeson
import Data.Semigroup (Last(..))
import Restylers.Image
import Restylers.Info (RestylerInfo)
import qualified Restylers.Info as Info
import Restylers.Name
import Restylers.Options
import qualified Restylers.Override as Override

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
    deriving anyclass (FromJSON, ToJSON)

instance Display Restyler where
    display Restyler { enabled, name, image, command, arguments, include, interpreters, supports_arg_sep, supports_multiple_paths, documentation }
        = display name
            <> " ("
            <> (if enabled then "enabled)" else "disabled)")
            <> ": "
            <> display image
            <> "\n"
            <> displayShow command
            <> " "
            <> displayShow arguments
            <> (if supports_arg_sep then " -- " else " ")
            <> (if supports_multiple_paths then "<path...>" else "<path>")
            <> "\n"
            <> displayShow (length include)
            <> " include pattern(s)"
            <> "\n"
            <> displayShow (length interpreters)
            <> " interpreters(s)"
            <> "\n"
            <> displayShow (length documentation)
            <> " link(s)"

loadRestylerInfo
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasOptions env)
    => FilePath
    -> (RestylerInfo -> m a)
    -> m (RestylerInfo, a)
loadRestylerInfo path mkMeta = do
    mOverride <- Override.load path

    case mOverride of
        Nothing -> do
            info <- Info.load path
            meta <- mkMeta info
            pure (info, meta)
        Just override -> do
            info <- Override.loadInfo override
            meta <- mkMeta info
            pure (info <> Override.toInfo override, meta)

mkDevImage
    :: (MonadReader env m, HasOptions env) => RestylerInfo -> m RestylerImage
mkDevImage info = do
    Options {..} <- view optionsL
    let name = getLast $ Info.name info
    pure $ mkRestylerImage oRegistry name oTag

mkRestyler :: RestylerInfo -> RestylerImage -> Restyler
mkRestyler info image = do
    Restyler
        { enabled = fromMaybeLast False $ Info.enabled info
        , name
        , image
        , command = fromMaybeLast [unRestylerName name] $ Info.command info
        , arguments = fromMaybeLast [] $ Info.arguments info
        , include = fromMaybeLast [] $ Info.include info
        , interpreters = fromMaybeLast [] $ Info.interpreters info
        , supports_arg_sep = fromMaybeLast True $ Info.supports_arg_sep info
        , supports_multiple_paths = fromMaybeLast True
            $ Info.supports_multiple_paths info
        , documentation = fromMaybeLast [] $ Info.documentation info
        }
    where name = getLast $ Info.name info

fromMaybeLast :: a -> Maybe (Last a) -> a
fromMaybeLast def = getLast . fromMaybe (Last def)
