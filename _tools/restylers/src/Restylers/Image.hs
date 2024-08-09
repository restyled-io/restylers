-- |
--
-- Module      : Restylers.Image
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Image
  ( RestylerImage
  , unRestylerImage
  , mkRestylerImage
  , getSeriesImages

    -- * Exported to test
  , chopFromEnd
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
  { registry :: Maybe Registry
  , name :: RestylerImageName
  , tag :: RestylerImageTag
  }
  deriving stock (Eq, Show)

newtype RestylerImageName = RestylerImageName
  { unwrap :: Text
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype RestylerImageTag = RestylerImageTag
  { unwrap :: Text
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

instance FromJSON RestylerImage where
  parseJSON = withText "RestylerImage" $ \full -> do
    (pre1, tag) <- invalidImage full ":" $ chopFromEnd ':' full
    (pre2, name) <- invalidImage full "right-most /" $ chopFromEnd '/' pre1
    (registry, org) <- invalidImage full "next /" $ chopFromEnd '/' pre2

    pure
      RestylerImage
        { registry = Registry <$> guarded (not . T.null) registry
        , name = RestylerImageName $ org <> "/" <> name
        , tag = RestylerImageTag tag
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
    { registry = registry
    , name = RestylerImageName $ "restyled/restyler-" <> name.unwrap
    , tag = RestylerImageTag tag
    }

unRestylerImage :: RestylerImage -> Text
unRestylerImage image =
  maybe "" ((<> "/") . (.unwrap)) image.registry
    <> image.name.unwrap
    <> ":"
    <> image.tag.unwrap

setRestylerImageTag :: RestylerImage -> RestylerImageTag -> RestylerImage
setRestylerImageTag i t = i {tag = t}

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
  version = RestylerVersion $ image.tag.unwrap

guarded :: Alternative f => (t -> Bool) -> t -> f t
guarded p x = x <$ guard (p x)
