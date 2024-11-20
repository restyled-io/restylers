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
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Text.IO qualified as T
import Data.Yaml qualified as Yaml
import Restylers.Env
import Restylers.Info.Metadata qualified as Metadata
import Restylers.Info.Test
  ( testDescription
  , testFilePath
  , writeTestFiles
  )
import Restylers.Info.Test qualified as Test
import Restylers.Info.Test.Support (withSupportFile)
import Restylers.Manifest qualified as Manifest
import Restylers.Name (RestylerName (..))
import System.Environment (withArgs)
import System.FilePath (takeBaseName, (</>))
import System.Process.Typed
import Test.Hspec
import UnliftIO.Directory
import UnliftIO.Temporary (withTempDirectory)

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
  cwd <- getCurrentDirectory
  rts <- (.testShow) <$> getRestylersEnv

  withTempDirectory cwd "restylers-test" $ \tmp ->
    withCurrentDirectory tmp $ do
      crestylers <- for restylers $ \restyler -> do
        files <- for (restylerTests restyler) $ \(number, test) -> do
          writeTestFiles number restyler.name restyler.include test
        pure $ CRestyler restyler.name files

      writeYaml testManifest restylers
      writeYaml ".restyled.yaml"
        $ object
          [ "restylers_version" .= ("testing" :: Text)
          , "restylers" .= crestylers
          ]

      let code = cwd </> takeBaseName tmp

      flushLogger -- before hspec makes its own output
      liftIO $ do
        withArgs hspecArgs $ hspec $ do
          for_ restylers $ \restyler -> do
            describe (unpack $ restyler.name.unwrap) $ do
              for_ (restylerTests restyler) $ \(number, test) -> do
                it (testDescription number test) $ do
                  (ec, out, err) <-
                    maybe id withSupportFile test.support
                      $ readProcess
                      $ proc "restyle"
                      $ concat
                        [ ["--debug"]
                        , ["--color", "always"]
                        , ["--host-directory", code]
                        , ["--manifest", testManifest]
                        , ["--no-commit"]
                        , ["--no-clean"]
                        , ["--no-pull" | not pull]
                        , ["--restyler-memory", "512m"]
                        , ["."]
                        ]

                  when (ec /= ExitSuccess) $ do
                    throwString
                      $ unlines
                        [ "Restyler " <> show ec
                        , "stdout:"
                        , BSL8.unpack out
                        , ""
                        , "stderr:"
                        , BSL8.unpack err
                        , ""
                        ]

                  restyled <- T.readFile $ testFilePath number restyler.name restyler.include test

                  when (restyled == test.contents) $ do
                    expectationFailure
                      $ unlines
                        [ "Restyler made no changes to the given files. For debugging, output was:"
                        , "stdout:"
                        , BSL8.unpack out
                        , ""
                        , "stderr:"
                        , BSL8.unpack err
                        , ""
                        ]

                  if rts
                    then show restyled `shouldBe` show test.restyled
                    else restyled `shouldBe` test.restyled

restylerTests :: Manifest.Restyler -> [(Int, Test.Test)]
restylerTests r = zip [1 ..] r.metadata.tests

writeYaml :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeYaml path =
  liftIO
    . T.writeFile path
    . decodeUtf8With lenientDecode
    . Yaml.encode

testManifest :: FilePath
testManifest = "./manifest.yaml"
