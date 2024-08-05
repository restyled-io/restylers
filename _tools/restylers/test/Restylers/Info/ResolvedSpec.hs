{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restylers.Info.ResolvedSpec
  ( spec
  ) where

import RIO

import RIO.Directory (createDirectoryIfMissing, withCurrentDirectory)
import RIO.FilePath (takeDirectory, (<.>), (</>))
import qualified RIO.Text as T
import Restylers.Info.Build (restylerBuild)
import Restylers.Info.Resolved (ImageSource (..))
import qualified Restylers.Info.Resolved as Info
import Restylers.Name
import Restylers.Options
import Restylers.Version
import Test.Hspec

instance HasOptions SimpleApp where
  optionsL = lens testOptions undefined

testOptions :: a -> Options
testOptions _ =
  Options
    { oRegistry = Nothing
    , oSha = "dev"
    , oDebug = False
    , oBuild = False
    , oPush = False
    , oWrite = Nothing
    , oCheckForUpdate = False
    , oInput = undefined
    , oHspecArgs = Nothing
    }

spec :: Spec
spec = do
  describe "load" $ do
    it "can load an override with versioned image" $ inTempDirectory $ do
      let base = "prettier" </> "info" <.> "yaml"
      addFile base
        $ T.unlines
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
      let override = "prettier-json" </> "info" <.> "yaml"
      addFile override
        $ T.unlines
          [ "overrides: prettier"
          , "name: prettier-json"
          , "include:"
          , "  - \"**/*.json\""
          , "documentation:"
          , "  - https://prettier.io/docs/en/options.html#parser"
          ]

      info <- runSimpleApp $ Info.load override

      Info.name info `shouldBe` RestylerName "prettier-json"
      Info.imageSource info
        `shouldBe` BuildVersion
          (RestylerName "prettier")
          (RestylerVersion "v2.0.2-2")
          (restylerBuild base)
      Info.command info `shouldBe` ["prettier", "--write"]
      Info.include info `shouldBe` ["**/*.json"]

inTempDirectory :: MonadUnliftIO m => m a -> m a
inTempDirectory f =
  withSystemTempDirectory "" $ \tmp -> withCurrentDirectory tmp f

addFile :: MonadIO m => FilePath -> Text -> m ()
addFile path contents = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFileUtf8 path contents
