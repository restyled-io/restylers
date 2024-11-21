-- |
--
-- Module      : Restylers.Test
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Test
  ( testRestylers
  ) where

import Restylers.Prelude

import Blammo.Logging.Logger (flushLogger)
import Data.Aeson (ToJSON, object)
import Data.Text.IO qualified as T
import Data.Yaml qualified as Yaml
import Restylers.Env
import Restylers.Info.Metadata qualified as Metadata
import Restylers.Info.Test
  ( testDescription
  , writeTestFile
  , writeTestSupportFile
  )
import Restylers.Info.Test qualified as Test
import Restylers.Manifest qualified as Manifest
import Restylers.Name (RestylerName (..))
import Restylers.Test.Output
import System.Environment (withArgs)
import System.Process.Typed
import Test.Hspec
import UnliftIO.Directory
import UnliftIO.Temporary (withSystemTempDirectory)

data CRestyler = CRestyler
  { name :: RestylerName
  , include :: [FilePath]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

testRestylers
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => Bool
  -> NonEmpty Manifest.Restyler
  -> [String]
  -> m ()
testRestylers pull restylers hspecArgs = do
  rts <- (.testShow) <$> getRestylersEnv

  flushLogger -- before hspec makes its own output
  liftIO $ do
    withArgs hspecArgs $ hspec $ do
      for_ restylers $ \restyler -> do
        describe (unpack $ restyler.name.unwrap) $ do
          for_ (restylerTests restyler) $ \(number, test) -> do
            it (testDescription number test) $ do
              inTempDirectory $ do
                writeYaml testManifest restylers
                writeTestSupportFile test

                file <- writeTestFile number restyler.name restyler.include test

                writeYaml ".restyled.yaml"
                  $ object
                    [ "restylers_version" .= ("testing" :: Text)
                    , "restylers"
                        .= [ restyler
                              { Manifest.enabled = True
                              , Manifest.include = [pack file]
                              , Manifest.command = fromMaybe restyler.command test.command
                              , Manifest.arguments = fromMaybe restyler.arguments test.arguments
                              }
                           ]
                    ]

                (ec, out, err) <-
                  readProcess
                    $ proc "restyle"
                    $ concat
                      [ ["--debug"]
                      , ["--color", "always"]
                      , ["--manifest", testManifest]
                      , ["--no-commit"]
                      , ["--no-clean"]
                      , ["--no-pull" | not pull]
                      , ["--restyler-memory", "512m"]
                      , ["."]
                      ]

                when (ec /= ExitSuccess)
                  $ expectationFailure
                  $ "Restyler "
                  <> show ec
                  <> "\n"
                  <> frameOutput out err

                restyled <- T.readFile file

                when (restyled == test.contents)
                  $ expectationFailure
                  $ "Restyler made no changes to "
                  <> file
                  <> "\n"
                  <> frameOutput out err

                if rts
                  then show restyled `shouldBe` show test.restyled
                  else restyled `shouldBe` test.restyled

restylerTests :: Manifest.Restyler -> [(Int, Test.Test)]
restylerTests r = zip [1 ..] r.metadata.tests

inTempDirectory :: MonadUnliftIO m => m a -> m a
inTempDirectory f = do
  withSystemTempDirectory "restylers-test" $ \tmp ->
    withCurrentDirectory tmp f

writeYaml :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeYaml path =
  liftIO
    . T.writeFile path
    . decodeUtf8With lenientDecode
    . Yaml.encode

testManifest :: FilePath
testManifest = "./manifest.yaml"
