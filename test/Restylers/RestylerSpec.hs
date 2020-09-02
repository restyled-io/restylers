module Restylers.RestylerSpec
    ( spec
    )
where

import RIO

import Restylers.Image
import Restylers.Name
import qualified Restylers.Restyler as Restyler
import RIO.Directory (createDirectoryIfMissing, withCurrentDirectory)
import RIO.FilePath (takeDirectory)
import qualified RIO.Text as T
import Test.Hspec

spec :: Spec
spec = do
    describe "load" $ do
        it "can load an override restyler" $ do
            withSystemTempDirectory "" $ \tmp -> do
                withCurrentDirectory tmp $ do
                    let prettierYaml = "prettier/info.yaml"
                        prettierJsonYaml = "prettier-json/info.yaml"
                    addFile prettierYaml $ T.unlines
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
                    addFile prettierJsonYaml $ T.unlines
                        [ "overrides: prettier"
                        , "name: prettier-json"
                        , "include:"
                        , "  - \"**/*.json\""
                        , "documentation:"
                        , "  - https://prettier.io/docs/en/options.html#parser"
                        ]

                    restyler <- Restyler.loadInfo Nothing prettierJsonYaml

                    Restyler.name restyler
                        `shouldBe` RestylerName "prettier-json"
                    Restyler.image restyler `shouldBe` mkRestylerImage
                        Nothing
                        (RestylerName "prettier")
                        "v2.0.2-2"

addFile :: MonadIO m => FilePath -> Text -> m ()
addFile path contents = do
    createDirectoryIfMissing True $ takeDirectory path
    writeFileUtf8 path contents
