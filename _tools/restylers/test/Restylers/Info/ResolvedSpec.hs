{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restylers.Info.ResolvedSpec
  ( spec
  ) where

import Restylers.Prelude

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Restylers.Info.Build (restylerBuild)
import Restylers.Info.Resolved (ImageSource (..))
import Restylers.Info.Resolved qualified as Info
import Restylers.Name
import Restylers.Options
import Restylers.Version
import System.FilePath (takeDirectory, (<.>), (</>))
import Test.Hspec
import UnliftIO.Directory (createDirectoryIfMissing, withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

testOptions :: a -> Options
testOptions _ =
  Options
    { registry = Nothing
    , sha = "dev"
    , debug = False
    , build = False
    , pull = False
    , push = False
    , write = Nothing
    , checkForUpdate = False
    , input = undefined
    , hspecArgs = Nothing
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

      info <- runReaderT (Info.load override) testOptions

      info.name `shouldBe` RestylerName "prettier-json"
      info.imageSource
        `shouldBe` BuildVersion
          (RestylerName "prettier")
          (RestylerVersion "v2.0.2-2")
          (restylerBuild base)
      info.command `shouldBe` ["prettier", "--write"]
      info.include `shouldBe` ["**/*.json"]

inTempDirectory :: MonadUnliftIO m => m a -> m a
inTempDirectory f =
  withSystemTempDirectory "" $ \tmp -> withCurrentDirectory tmp f

addFile :: MonadIO m => FilePath -> Text -> m ()
addFile path contents = do
  createDirectoryIfMissing True $ takeDirectory path
  liftIO $ T.writeFile path contents
