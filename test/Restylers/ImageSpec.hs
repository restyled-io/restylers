module Restylers.ImageSpec
    ( spec
    )
where

import RIO

import Data.Aeson
import Restylers.Image
import Restylers.Name
import Restylers.Registry
import qualified RIO.ByteString.Lazy as BSL
import Test.Hspec

spec :: Spec
spec = do
    describe "unRestylerImage" $ do
        it "formats properly without registry" $ do
            let image = mkRestylerImage Nothing (RestylerName "foo") "bar"

            unRestylerImage image `shouldBe` "restyled/restyler-foo:bar"

        it "formats properly with registry" $ do
            let
                image = mkRestylerImage
                    (Just $ Registry "quay.io")
                    (RestylerName "foo")
                    "bar"

            unRestylerImage image `shouldBe` "quay.io/restyled/restyler-foo:bar"

    describe "FromJSON" $ do
        it "accepts valid images without registry" $ do
            let image = mkRestylerImage Nothing (RestylerName "foo") "bar"

            decodeTextValue (unRestylerImage image) `shouldBe` Right image

        it "accepts valid images with registry" $ do
            let
                image = mkRestylerImage
                    (Just $ Registry "quay.io")
                    (RestylerName "foo")
                    "bar"

            decodeTextValue (unRestylerImage image) `shouldBe` Right image

        it "rejects images without :tag" $ do
            decodeTextValue @RestylerImage "foo/bar" `shouldSatisfy` isLeft

        it "rejects images without org/name" $ do
            decodeTextValue @RestylerImage "bar:baz" `shouldSatisfy` isLeft

decodeTextValue :: FromJSON a => Text -> Either String a
decodeTextValue x =
    eitherDecode $ BSL.fromStrict $ encodeUtf8 $ "\"" <> x <> "\""
