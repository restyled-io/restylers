module Restylers.Info
    ( RestylerInfo(..)
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

data RestylerInfo = RestylerInfo
    { name :: RestylerName
    , image :: Registry -> RestylerImage
    , command :: [Text]
    , arguments :: [Text]
    , supports_arg_sep :: Bool
    , supports_multiple_paths :: Bool
    , metadata :: Metadata
    }
    deriving stock Generic

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

data Metadata = Metadata
    { languages :: [Text]
    , tests :: [Test]
    }
    deriving stock Generic
    deriving anyclass FromJSON

emptyMetadata :: Metadata
emptyMetadata = Metadata [] []

data Test = Test
    { extension :: Maybe Text
    , contents :: Text
    , restyled :: Text
    }
    deriving stock Generic
    deriving anyclass FromJSON

load :: MonadIO m => FilePath -> m RestylerInfo
load = Yaml.decodeFileThrow
