{-# LANGUAGE NamedFieldPuns #-}

module Restylers.Restyler
    ( Restyler(..)
    , loadInfo
    )
where

import RIO

import Data.Aeson
import Data.Semigroup (Last(..))
import qualified Data.Yaml as Yaml
import Restylers.Image
import Restylers.Info (RestylerInfo, restylerInfoYaml)
import qualified Restylers.Info as Info
import Restylers.Name
import Restylers.Override (RestylerOverride)
import qualified Restylers.Override as Override
import Restylers.Registry
import Restylers.Version
import RIO.Text (unpack)

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
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

loadInfo :: MonadIO m => Maybe Registry -> FilePath -> m Restyler
loadInfo registry path = liftIO $ do
    eOverride <- Yaml.decodeFileEither path
    either
        (\_ -> do
            info <- Yaml.decodeFileThrow path
            inflateInfo registry info Nothing
        )
        (\override -> do
            base <- loadOverriddenInfo override
            let info = base <> Override.overridingInfo override
            inflateInfo registry info $ Just $ Override.overrides override
        )
        eOverride

loadOverriddenInfo :: MonadIO m => RestylerOverride -> m RestylerInfo
loadOverriddenInfo override =
    Yaml.decodeFileThrow $ restylerInfoYaml $ Override.overrides override

inflateInfo
    :: MonadIO m
    => Maybe Registry
    -> RestylerInfo
    -> Maybe RestylerName
    -> m Restyler
inflateInfo registry info overridden = do
    image <- either (throwString . prefixError) pure $ mkImplicitImage
        registry
        nameForImplicitImage
        (getLast <$> Info.version info)
        (getLast <$> Info.image info)

    pure Restyler
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
  where
    name = getLast $ Info.name info
    nameForImplicitImage = fromMaybe name overridden
    prefixError msg = restylerInfoYaml name <> ": " <> unpack msg

mkImplicitImage
    :: Maybe Registry
    -> RestylerName
    -> Maybe RestylerVersion
    -> Maybe RestylerImage
    -> Either Text RestylerImage
mkImplicitImage _ _ Nothing Nothing =
    Left "one of version or image is required"
mkImplicitImage _ _ _ (Just image) = pure image
mkImplicitImage registry name (Just version) Nothing =
    Right $ mkRestylerImage registry name $ unRestylerVersion version

fromMaybeLast :: a -> Maybe (Last a) -> a
fromMaybeLast def = getLast . fromMaybe (Last def)
