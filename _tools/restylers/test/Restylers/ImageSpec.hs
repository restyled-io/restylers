module Restylers.ImageSpec
  ( spec
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Either (isLeft)
import Data.Maybe (isNothing)
import Restylers.Image
import Restylers.Name
import Restylers.Registry
import Test.Hspec

spec :: Spec
spec = do
  describe "unRestylerImage" $ do
    it "formats properly without registry" $ do
      let image = mkRestylerImage Nothing (RestylerName "foo") "bar"

      unRestylerImage image `shouldBe` "restyled/restyler-foo:bar"

    it "formats properly with registry" $ do
      let image =
            mkRestylerImage
              (Just $ Registry "quay.io")
              (RestylerName "foo")
              "bar"

      unRestylerImage image `shouldBe` "quay.io/restyled/restyler-foo:bar"

  describe "FromJSON" $ do
    it "accepts valid images without registry" $ do
      let image = mkRestylerImage Nothing (RestylerName "foo") "bar"

      decodeTextValue (unRestylerImage image) `shouldBe` Right image

    it "accepts valid images with registry" $ do
      let image =
            mkRestylerImage
              (Just $ Registry "quay.io")
              (RestylerName "foo")
              "bar"

      decodeTextValue (unRestylerImage image) `shouldBe` Right image

    it "rejects images without :tag" $ do
      decodeTextValue @RestylerImage "foo/bar" `shouldSatisfy` isLeft

    it "rejects images without org/name" $ do
      decodeTextValue @RestylerImage "bar:baz" `shouldSatisfy` isLeft

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
        toImage = mkRestylerImage Nothing (RestylerName "foo")

        expected :: Maybe (NonEmpty Text)
        expected = fmap (unRestylerImage . toImage) <$> mSeriesTags

        actual :: Maybe (NonEmpty Text)
        actual = fmap unRestylerImage <$> getSeriesImages (toImage tag)

        doc =
          if isNothing mSeriesTags
            then "does not expand " <> unpack tag
            else "expands " <> unpack tag <> " into series images"

      it doc $ actual `shouldBe` expected

  describe "chopFromEnd" $ do
    it "works" $ do
      chopFromEnd ':' "foo/bar/baz/bat:quix"
        `shouldBe` Just ("foo/bar/baz/bat", "quix")
      chopFromEnd ':' "foo/bar/baz/bat" `shouldBe` Just ("", "foo/bar/baz/bat")
      chopFromEnd ':' "foo/bar/baz/bat:" `shouldBe` Nothing
      chopFromEnd '/' "foo/bar" `shouldBe` Just ("foo", "bar")
      chopFromEnd '/' "foo/bar/baz" `shouldBe` Just ("foo/bar", "baz")
      chopFromEnd '/' "foo" `shouldBe` Just ("", "foo")

decodeTextValue :: FromJSON a => Text -> Either String a
decodeTextValue x =
  eitherDecode $ BSL.fromStrict $ encodeUtf8 $ "\"" <> x <> "\""
