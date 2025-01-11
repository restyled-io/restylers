-- |
--
-- Module      : Restylers.ImageSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.ImageSpec
  ( spec
  )
where

import Restylers.Prelude

import Data.Maybe (isNothing)
import Restylers.Image
import Restylers.Name
import Test.Hspec

spec :: Spec
spec = do
  describe "getSeriesImages" $ do
    let
      testCases :: [(Text, Maybe (NonEmpty Text))]
      testCases =
        [ ("v2.0.2", Just ("v2" :| ["v2.0"]))
        , ("15.0.7", Nothing)
        , ("v0.6.1-alpha-3", Just ("v0" :| ["v0.6"]))
        , ("v0.13.0.0", Nothing)
        , ("v2", Nothing)
        , ("go1.20.5", Nothing)
        , ("v5.3", Nothing)
        , ("v0.0-1318-gf6b4485", Nothing)
        ]

    for_ testCases $ \(tag, mSeriesTags) -> do
      let
        toImage :: Text -> RestylerImage
        toImage =
          either error id . mkRestylerImage "restyled/" (RestylerName "foo")

        expected :: Maybe (NonEmpty Text)
        expected = fmap (unRestylerImage . toImage) <$> mSeriesTags

        actual :: Maybe (NonEmpty Text)
        actual = fmap unRestylerImage <$> getSeriesImages (toImage tag)

        doc =
          if isNothing mSeriesTags
            then "does not expand " <> unpack tag
            else "expands " <> unpack tag <> " into series images"

      it doc $ actual `shouldBe` expected
