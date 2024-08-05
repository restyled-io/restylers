{-# LANGUAGE RecordWildCards #-}

module Restylers.Image
  ( RestylerImage
  , unRestylerImage
  , mkRestylerImage
  , getSeriesImages
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Restylers.Name
import Restylers.Registry
import Restylers.Version

data RestylerImage = RestylerImage
  { riRegistry :: Maybe Registry
  , riName :: RestylerImageName
  , riTag :: RestylerImageTag
  }
  deriving stock (Eq, Show)

newtype RestylerImageName = RestylerImageName
  { unRestylerImageName :: Text
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype RestylerImageTag = RestylerImageTag
  { unRestylerImageTag :: Text
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

instance FromJSON RestylerImage where
  parseJSON = withText "RestylerImage" $ \full -> do
    (pre1, tag) <- invalidImage full ":" $ chopFromEnd ':' full
    (pre2, name) <- invalidImage full "right-most /" $ chopFromEnd '/' pre1
    (registry, org) <- invalidImage full "next /" $ chopFromEnd '/' pre2

    pure
      RestylerImage
        { riRegistry = Registry <$> guarded (not . T.null) registry
        , riName = RestylerImageName $ org <> "/" <> name
        , riTag = RestylerImageTag tag
        }
   where
    invalidImage :: MonadFail m => Text -> String -> Maybe a -> m a
    invalidImage x msg =
      maybe
        ( fail
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
chopFromEnd :: Char -> Text -> Maybe (Text, Text)
chopFromEnd c value = case T.breakOnEnd (T.singleton c) value of
  (_, y) | T.null y -> Nothing
  (x, y) -> Just (T.dropWhileEnd (== c) x, y)

instance ToJSON RestylerImage where
  toJSON = toJSON . unRestylerImage
  toEncoding = toEncoding . unRestylerImage

mkRestylerImage :: Maybe Registry -> RestylerName -> Text -> RestylerImage
mkRestylerImage registry name tag =
  RestylerImage
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

setRestylerImageTag :: RestylerImage -> RestylerImageTag -> RestylerImage
setRestylerImageTag i t = i {riTag = t}

getSeriesImages :: RestylerImage -> Maybe (NonEmpty RestylerImage)
getSeriesImages image = do
  tags <-
    NE.nonEmpty
      $ catMaybes
        [ toSeriesMajor version
        , toSeriesMinor version
        ]
  pure $ setRestylerImageTag image . RestylerImageTag <$> tags
 where
  version = RestylerVersion $ unRestylerImageTag $ riTag image

guarded :: Alternative f => (t -> Bool) -> t -> f t
guarded p x = x <$ guard (p x)
