{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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
  , mkRestylerImageThrow
  , getSeriesImages
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Restylers.Name
import Restylers.Version

data RestylerImage = RestylerImage
  { name :: RestylerImageName
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
  parseJSON = withText "RestylerImage" $ either fail pure . parseRestylerImage

instance ToJSON RestylerImage where
  toJSON = toJSON . unRestylerImage
  toEncoding = toEncoding . unRestylerImage

mkRestylerImage :: Text -> RestylerName -> Text -> Either String RestylerImage
mkRestylerImage prefix name tag =
  parseRestylerImage
    $ addTrailingSeparator prefix
    <> "restyler-"
    <> name.unwrap
    <> ":"
    <> tag
 where
  addTrailingSeparator x
    | "/" `T.isSuffixOf` x = x
    | otherwise = x <> "/"

mkRestylerImageThrow
  :: MonadIO m => Text -> RestylerName -> Text -> m RestylerImage
mkRestylerImageThrow prefix name =
  either throwString pure . mkRestylerImage prefix name

parseRestylerImage :: Text -> Either String RestylerImage
parseRestylerImage full = do
  case T.breakOnEnd ":" full of
    (image', tag)
      | Just (image, ':') <- T.unsnoc image'
      , not $ T.null image
      , not $ T.null tag ->
          Right
            RestylerImage
              { name = RestylerImageName image
              , tag = RestylerImageTag tag
              }
    _ ->
      Left
        $ "Invalid image: "
        <> unpack full
        <> ", must match [HOST[:PORT_NUMBER]/]PATH:TAG"

unRestylerImage :: RestylerImage -> Text
unRestylerImage image = image.name.unwrap <> ":" <> image.tag.unwrap

setRestylerImageTag :: RestylerImage -> RestylerImageTag -> RestylerImage
setRestylerImageTag i tag = i {tag}

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
