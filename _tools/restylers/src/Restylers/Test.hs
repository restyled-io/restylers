module Restylers.Test
  ( testRestylers
  ) where

import Restylers.Prelude

import Data.Aeson (ToJSON, object)
import Data.ByteString.Lazy qualified as BSL
import Data.Text.IO qualified as T
import Data.Yaml qualified as Yaml
import Restylers.Info.Metadata qualified as Metadata
import Restylers.Info.Test
  ( testDescription
  , testFilePath
  , writeTestFiles
  )
import Restylers.Info.Test qualified as Test
import Restylers.Manifest qualified as Manifest
import Restylers.Name (RestylerName (..))
import System.Environment (lookupEnv, withArgs)
import System.FilePath (takeBaseName, (</>))
import System.Process.Typed
import Test.Hspec
import UnliftIO.Directory
import UnliftIO.Temporary (withTempDirectory)

testRestylers
  :: ( MonadUnliftIO m
     , MonadLogger m
     )
  => Bool
  -> NonEmpty Manifest.Restyler
  -> [String]
  -> m ()
testRestylers pull restylers hspecArgs = do
  cwd <- getCurrentDirectory
  rts <- liftIO $ maybe False (not . null) <$> lookupEnv "RESTYLERS_TEST_SHOW"

  withTempDirectory cwd "restylers-test" $ \tmp ->
    withCurrentDirectory tmp $ do
      for_ restylers $ \restyler -> do
        for_ (restylerTests restyler) $ \(number, test) -> do
          writeTestFiles
            number
            (Manifest.name restyler)
            (Manifest.include restyler)
            test

      writeYaml testManifest restylers
      writeYaml ".restyled.yaml"
        $ object
          [ "restylers_version" .= ("testing" :: Text)
          , "restylers" .= (Manifest.name <$> restylers)
          ]

      let code = cwd </> takeBaseName tmp

      delayedException <-
        tryAny $ do
          (out, err) <- runRestyler pull code
          logDebug
            $ (:# [])
            $ "stdout: "
            <> decodeUtf8With lenientDecode (BSL.toStrict out)
          logDebug
            $ (:# [])
            $ "stderr: "
            <> decodeUtf8With lenientDecode (BSL.toStrict err)

      liftIO $ do
        withArgs hspecArgs $ hspec $ do
          for_ restylers $ \restyler -> do
            describe (unpack $ unRestylerName $ Manifest.name restyler) $ do
              for_ (restylerTests restyler) $ \(number, test) -> do
                it (testDescription number test) $ do
                  -- If docker-run failed, re-throw it here so it's handled
                  void $ either throwIO pure delayedException
                  restyled <-
                    T.readFile
                      $ testFilePath
                        number
                        (Manifest.name restyler)
                        (Manifest.include restyler)
                        test

                  if rts
                    then show restyled `shouldBe` show (Test.restyled test)
                    else restyled `shouldBe` Test.restyled test

restylerTests :: Manifest.Restyler -> [(Int, Test.Test)]
restylerTests = zip [1 ..] . Metadata.tests . Manifest.metadata

runRestyler
  :: MonadIO m
  => Bool
  -> FilePath
  -> m (BSL.ByteString, BSL.ByteString)
runRestyler pull code = do
  readProcess_
    $ proc
      "restyle"
    $ concat
      [ ["--debug"]
      , ["--color", "always"]
      , ["--host-directory", code]
      , ["--manifest", testManifest]
      , ["--no-commit"]
      , ["--no-pull" | not pull]
      , ["."]
      ]

writeYaml :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeYaml path =
  liftIO
    . T.writeFile path
    . decodeUtf8With lenientDecode
    . Yaml.encode

testManifest :: FilePath
testManifest = "./manifest.yaml"
