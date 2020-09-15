{-# LANGUAGE NamedFieldPuns #-}

module Restylers.Info.Resolved
    ( RestylerInfo(..)
    , ImageSource(..)
    , load
    , restylerInfoYaml
    , restylerVersionCache
    )
where

import RIO

import Data.Aeson
import Data.Semigroup (Last(..))
import qualified Data.Yaml as Yaml
import Restylers.Image
import qualified Restylers.Info as Info
import Restylers.Info.Build (RestylerBuild, restylerBuild)
import Restylers.Info.Metadata (Metadata)
import qualified Restylers.Info.Metadata as Metadata
import Restylers.Name
import qualified Restylers.Override as Override
import Restylers.Version
import RIO.FilePath ((<.>), (</>))
import RIO.Text (unpack)

data RestylerInfo = RestylerInfo
    { enabled :: Bool
    , name :: RestylerName
    , command :: [Text]
    , arguments :: [Text]
    , include :: [Text]
    , interpreters :: [Text]
    , supports_arg_sep :: Bool
    , supports_multiple_paths :: Bool
    , documentation :: [Text]
    , metadata :: Metadata
    , imageSource :: ImageSource
    }

instance Display RestylerInfo where
    display = display . name

data ImageSource
    = Explicit RestylerImage
    | BuildVersionCmd RestylerName String RestylerBuild
    | BuildVersion RestylerName RestylerVersion RestylerBuild
    deriving stock (Eq, Show)

getImageSource :: MonadIO m => FilePath -> Info.RestylerInfo -> m ImageSource
getImageSource yaml info =
    case
            ( getLast $ Info.name info
            , getLast <$> Info.version info
            , getLast <$> Info.version_cmd info
            , getLast <$> Info.image info
            )
        of
            (name, Nothing, Nothing, Nothing) ->
                throwString
                    $ unpack (unRestylerName name)
                    <> ": one of image, version_cmd, or version must be specified"
            (_, Nothing, Nothing, Just image) -> pure $ Explicit image
            (name, Nothing, Just cmd, _) ->
                pure $ BuildVersionCmd name cmd build
            (name, Just version, _, _) ->
                pure $ BuildVersion name version build
    where build = fromMaybeLast (restylerBuild yaml) $ Info.build info

load :: MonadIO m => FilePath -> m RestylerInfo
load yaml = do
    eOverride <- liftIO $ Yaml.decodeFileEither yaml

    case eOverride of
        Left _ -> do
            info <- decodeYaml yaml
            imageSource <- getImageSource yaml info
            pure $ fromInfo info imageSource

        Right override -> do
            let overridesYaml = restylerInfoYaml $ Override.overrides override
            info <- decodeYaml overridesYaml
            imageSource <- getImageSource overridesYaml info
            pure $ fromInfo (info <> overrideToInfo override) imageSource

fromInfo :: Info.RestylerInfo -> ImageSource -> RestylerInfo
fromInfo info imageSource = RestylerInfo
    { enabled = fromMaybeLast False $ Info.enabled info
    , name
    , command = fromMaybeLast [unRestylerName name] $ Info.command info
    , arguments = fromMaybeLast [] $ Info.arguments info
    , include = fromMaybeLast [] $ Info.include info
    , interpreters = fromMaybeLast [] $ Info.interpreters info
    , supports_arg_sep = fromMaybeLast True $ Info.supports_arg_sep info
    , supports_multiple_paths = fromMaybeLast True
        $ Info.supports_multiple_paths info
    , documentation = fromMaybeLast [] $ Info.documentation info
    , metadata = fromMaybeLast Metadata.emptyMetadata $ Info.metadata info
    , imageSource
    }
    where name = getLast $ Info.name info

overrideToInfo :: Override.RestylerOverride -> Info.RestylerInfo
overrideToInfo Override.RestylerOverride { enabled, name, command, arguments, include, interpreters, supports_arg_sep, supports_multiple_paths, documentation, metadata }
    = Info.RestylerInfo
        { enabled
        , name
        , Info.version = Nothing
        , Info.version_cmd = Nothing
        , Info.image = Nothing
        , build = Nothing
        , command
        , arguments
        , include
        , interpreters
        , supports_arg_sep
        , supports_multiple_paths
        , documentation
        , metadata
        }

restylerInfoYaml :: RestylerName -> FilePath
restylerInfoYaml name =
    "restylers" </> unpack (unRestylerName name) </> "info" <.> "yaml"

restylerVersionCache :: RestylerName -> FilePath
restylerVersionCache name =
    "restylers" </> unpack (unRestylerName name) </> ".version"

decodeYaml :: (MonadIO m, FromJSON a) => FilePath -> m a
decodeYaml = liftIO . Yaml.decodeFileThrow

fromMaybeLast :: a -> Maybe (Last a) -> a
fromMaybeLast def = getLast . fromMaybe (Last def)
