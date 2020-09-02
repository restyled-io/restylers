module Restylers.RestylerSpec
    ( spec
    )
where

import RIO

import Restylers.Image
import Restylers.Info (restylerInfoYaml)
import Restylers.Name
import qualified Restylers.Restyler as Restyler
import RIO.Directory (createDirectoryIfMissing, withCurrentDirectory)
import RIO.FilePath (takeDirectory)
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "load" $ do
        it "can load an override restyler" $ inTempDirectory $ do
            let base = restylerInfoYaml $ RestylerName "prettier"
                override = restylerInfoYaml $ RestylerName "prettier-json"
            addFile base $ T.unlines
                [ "enabled: true"
                , "name: prettier"
                , "version: v2.0.2-2"
                , "command:"
                , "  - prettier"
                , "  - \"--write\""
                , "include:"
                , "  - \"**/*.js\""
                , "  - \"**/*.jsx\""
                , "documentation:"
                , "  - https://prettier.io/docs/en/"
                ]
            addFile override $ T.unlines
                [ "overrides: prettier"
                , "name: prettier-json"
                , "include:"
                , "  - \"**/*.json\""
                , "documentation:"
                , "  - https://prettier.io/docs/en/options.html#parser"
                ]

            restyler <- Restyler.loadInfo Nothing override

            Restyler.name restyler `shouldBe` RestylerName "prettier-json"
            Restyler.image restyler `shouldBe` mkRestylerImage
                Nothing
                (RestylerName "prettier")
                "v2.0.2-2"

inTempDirectory :: MonadUnliftIO m => m a -> m a
inTempDirectory f =
    withSystemTempDirectory "" $ \tmp -> withCurrentDirectory tmp f

addFile :: MonadIO m => FilePath -> Text -> m ()
addFile path contents = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFileUtf8 path contents
