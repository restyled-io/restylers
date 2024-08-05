module Restylers.Test
  ( testRestylers
  ) where

import RIO

import Data.Aeson (ToJSON, object, (.=))
import qualified Data.Yaml as Yaml
import qualified RIO.ByteString.Lazy as BSL
import RIO.FilePath (takeBaseName, (</>))
import RIO.Process
import RIO.Text (unpack)
import Restylers.Directory
import qualified Restylers.Info.Metadata as Metadata
import Restylers.Info.Test
  ( testDescription
  , testFilePath
  , writeTestFiles
  )
import qualified Restylers.Info.Test as Test
import qualified Restylers.Manifest as Manifest
import Restylers.Name (RestylerName (..))
import System.Environment (lookupEnv, withArgs)
import Test.Hspec

testRestylers
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     )
  => Bool
  -> NonEmpty Manifest.Restyler
  -> [String]
  -> m ()
testRestylers pull restylers hspecArgs = do
  cwd <- getCurrentDirectory
  chd <- getCurrentHostDirectory
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

      let code = chd </> takeBaseName tmp

      delayedException <-
        tryAny $ do
          (out, err) <- runRestyler pull code
          logDebug $ "stdout: " <> displayBytesUtf8 (BSL.toStrict out)
          logDebug $ "stderr: " <> displayBytesUtf8 (BSL.toStrict err)

      liftIO $ do
        withArgs hspecArgs $ hspec $ do
          for_ restylers $ \restyler -> do
            describe (unpack $ unRestylerName $ Manifest.name restyler) $ do
              for_ (restylerTests restyler) $ \(number, test) -> do
                it (testDescription number test) $ do
                  -- If docker-run failed, re-throw it here so it's handled
                  void $ either throwIO pure delayedException
                  restyled <-
                    readFileUtf8
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
  :: ( MonadIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     )
  => Bool
  -> FilePath
  -> m (BSL.ByteString, BSL.ByteString)
runRestyler pull code = do
  proc
    "restyle"
    ( concat
        [ ["--debug"]
        , ["--color", "always"]
        , ["--host-directory", code]
        , ["--manifest", testManifest]
        , ["--no-commit"]
        , ["--no-pull" | not pull]
        , ["."]
        ]
    )
    readProcess_

writeYaml :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeYaml path =
  writeFileUtf8 path
    . decodeUtf8With lenientDecode
    . Yaml.encode

testManifest :: FilePath
testManifest = "./manifest.yaml"
