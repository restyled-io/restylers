{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Restylers.Info
    ( RestylerInfo(..)
    , restylerInfoYaml
    , Metadata(..)
    , Test(..)
    , load
    )
where

import RIO

import Data.Aeson
import qualified Data.Yaml as Yaml
import Restylers.Image
import Restylers.Name
import Restylers.Registry
import RIO.FilePath ((</>))
import qualified RIO.HashMap as HashMap
import RIO.Text (unpack)

data RestylerInfo = RestylerInfo
    { name :: RestylerName
    , image :: Registry -> RestylerImage
    , command :: [Text]
    , arguments :: [Text]
    , supports_arg_sep :: Bool
    , supports_multiple_paths :: Bool
    , metadata :: Metadata
    }

instance FromJSON RestylerInfo where
    parseJSON = withObject "RestylerInfo" $ \o -> do
        name <- o .: "name"
        mVersion <- o .:? "version"
        mImage <- o .:? "image"
        image <- case (mVersion, mImage) of
            (Nothing, Nothing) -> fail "One of version or image is required"
            (_, Just i) -> pure $ const i
            (Just v, _) -> pure $ \registry ->
                RestylerImage
                    $ unRegistry registry
                    <> "/restyler-"
                    <> unRestylerName name
                    <> ":"
                    <> v
        command <- o .:? "command" .!= [unRestylerName name]
        arguments <- o .:? "arguments" .!= []
        supports_arg_sep <- o .:? "supports_arg_sep" .!= True
        supports_multiple_paths <- o .:? "supports_multiple_paths" .!= True
        metadata <- o .:? "metadata" .!= emptyMetadata
        pure RestylerInfo { .. }

data RestylerOverride = RestylerOverride
    { overrides :: RestylerName
    , details :: Value
    }

instance FromJSON RestylerOverride where
    parseJSON = withObject "RestylerOverride" $ \o -> do
        overrides <- o .: "overrides"
        let details = Object o
        pure RestylerOverride { .. }

data Metadata = Metadata
    { languages :: [Text]
    , tests :: [Test]
    }
    deriving stock Generic
    deriving anyclass FromJSON

data Test = Test
    { extension :: Maybe Text
    , contents :: Text
    , restyled :: Text
    }
    deriving stock Generic
    deriving anyclass FromJSON

restylerInfoYaml :: RestylerName -> FilePath
restylerInfoYaml = (</> "info.yaml") . unpack . unRestylerName

load :: MonadIO m => FilePath -> m RestylerInfo
load path = liftIO $ do
    eOverride <- Yaml.decodeFileEither path

    case eOverride of
        Left _ -> Yaml.decodeFileThrow path
        Right RestylerOverride { overrides, details } -> do
            base <- Yaml.decodeFileThrow $ restylerInfoYaml overrides
            case fromJSON $ unionValues base details of
                Error msg ->
                    throwString
                        $ "Failed to parse overridden Restyler Value as JSON: "
                        <> msg
                Success x -> pure x

unionValues :: Value -> Value -> Value
unionValues (Object hm1) (Object hm2) =
    Object $ HashMap.unionWith unionValues hm1 hm2
unionValues x _ = x

emptyMetadata :: Metadata
emptyMetadata = Metadata [] []
