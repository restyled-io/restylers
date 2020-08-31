module Restylers.Image
    ( RestylerImage
    , mkRestylerImage
    , unRestylerImage
    , dockerHubImageExists
    )
where

import RIO

import Data.Aeson
import Network.HTTP.Simple
import Network.HTTP.Types.Status (status200)
import Restylers.Name
import Restylers.Registry
import RIO.Text (unpack)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOnEnd)

data RestylerImage = RestylerImage
    { riRegistry :: Maybe Registry
    , riName :: RestylerImageName
    , riTag :: RestylerImageTag
    }
    deriving stock (Eq, Show)

newtype RestylerImageName = RestylerImageName
    { unRestylerImageName :: Text
    }
    deriving newtype (Eq, Show, Display, FromJSON, ToJSON)

newtype RestylerImageTag = RestylerImageTag
    { unRestylerImageTag :: Text
    }
    deriving newtype (Eq, Show, Display, FromJSON, ToJSON)

instance Display RestylerImage where
    display = display . unRestylerImage

instance FromJSON RestylerImage where
    parseJSON = withText "RestylerImage" $ \full -> do
        (pre1, tag) <- invalidImage full ":" $ chopFromEnd ':' full
        (pre2, name) <- invalidImage full "right-most /" $ chopFromEnd '/' pre1
        (registry, org) <- invalidImage full "next /" $ chopFromEnd '/' pre2

        pure RestylerImage
            { riRegistry = Registry <$> guarded (not . T.null) registry
            , riName = RestylerImageName $ org <> "/" <> name
            , riTag = RestylerImageTag tag
            }

      where
        invalidImage x msg = maybe
            (fail
            $ "Image was not well-formed. Expected "
            <> unpack x
            <> " to match ({registry}/){org}/{name}:{tag}."
            <> " Failed looking for "
            <> msg
            )
            pure

-- |
--
-- >>> chopFromEnd ':' "foo/bar/baz/bat:quix"
-- Just ("foo/bar/baz/bat","quix")
--
-- >>> chopFromEnd ':' "foo/bar/baz/bat"
-- Just ("","foo/bar/baz/bat")
--
-- >>> chopFromEnd ':' "foo/bar/baz/bat:"
-- Nothing
--
-- >>> chopFromEnd '/' "foo/bar"
-- Just ("foo","bar")
--
-- >>> chopFromEnd '/' "foo/bar/baz"
-- Just ("foo/bar","baz")
--
-- >>> chopFromEnd '/' "foo"
-- Just ("","foo")
--
chopFromEnd :: Char -> Text -> Maybe (Text, Text)
chopFromEnd c value = case T.breakOnEnd (T.singleton c) value of
    (_, y) | T.null y -> Nothing
    (x, y) -> Just (T.dropWhileEnd (== c) x, y)

instance ToJSON RestylerImage where
    toJSON = toJSON . unRestylerImage
    toEncoding = toEncoding . unRestylerImage

mkRestylerImage :: Maybe Registry -> RestylerName -> Text -> RestylerImage
mkRestylerImage registry name tag = RestylerImage
    { riRegistry = registry
    , riName = RestylerImageName $ "restyled/restyler-" <> unRestylerName name
    , riTag = RestylerImageTag tag
    }

unRestylerImage :: RestylerImage -> Text
unRestylerImage RestylerImage {..} =
    maybe "" ((<> "/") . unRegistry) riRegistry
        <> unRestylerImageName riName
        <> ":"
        <> unRestylerImageTag riTag

dockerHubImageExists
    :: (MonadIO m, MonadReader env m, HasLogFunc env) => RestylerImage -> m Bool
dockerHubImageExists RestylerImage {..} = do
    when (isJust riRegistry)
        $ throwString
        $ "Unable to use dockerHubImageExists with explicit Registry."
        <> " If we're moving to a new registry, we have to figure out how to"
        <> " check for existence on it."

    logDebug $ "Checking " <> fromString indexUrl
    let req = parseRequest_ indexUrl
    resp <- httpNoBody $ setRequestIgnoreStatus req
    let status = getResponseStatus resp
    logDebug $ "Status " <> displayShow status
    pure $ status == status200
  where
    indexUrl =
        unpack
            $ "https://index.docker.io/v1/repositories/"
            <> unRestylerImageName riName
            <> "/tags/"
            <> unRestylerImageTag riTag

guarded :: Alternative f => (t -> Bool) -> t -> f t
guarded p x = x <$ guard (p x)
