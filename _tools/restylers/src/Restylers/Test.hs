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
import Data.ByteString.Lazy qualified as BSL
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
import Restylers.Manifest qualified as Manifest
import Restylers.Name (RestylerName (..))
import System.Environment (withArgs)
import System.FilePath (takeBaseName, (</>))
import System.Process.Typed
import Test.Hspec
import UnliftIO.Directory
import UnliftIO.Temporary (withTempDirectory)

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
      for_ restylers $ \restyler -> do
        for_ (restylerTests restyler) $ \(number, test) -> do
          writeTestFiles number restyler.name restyler.include test

      writeYaml testManifest restylers
      writeYaml ".restyled.yaml"
        $ object
          [ "restylers_version" .= ("testing" :: Text)
          , "restylers" .= ((.name) <$> restylers)
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

      flushLogger -- before hspec makes its own output
      liftIO $ do
        withArgs hspecArgs $ hspec $ do
          for_ restylers $ \restyler -> do
            describe (unpack $ restyler.name.unwrap) $ do
              for_ (restylerTests restyler) $ \(number, test) -> do
                it (testDescription number test) $ do
                  -- If docker-run failed, re-throw it here so it's handled
                  void $ either throwIO pure delayedException
                  restyled <- T.readFile $ testFilePath number restyler.name restyler.include test

                  if rts
                    then show restyled `shouldBe` show test.restyled
                    else restyled `shouldBe` test.restyled

restylerTests :: Manifest.Restyler -> [(Int, Test.Test)]
restylerTests r = zip [1 ..] r.metadata.tests

runRestyler
  :: (MonadIO m, MonadLogger m, MonadReader env m, HasLogger env)
  => Bool
  -> FilePath
  -> m (BSL.ByteString, BSL.ByteString)
runRestyler pull code = do
  p <-
    loggedProc "restyle"
      $ concat
        [ ["--debug"]
        , ["--color", "always"]
        , ["--host-directory", code]
        , ["--manifest", testManifest]
        , ["--no-commit"]
        , ["--no-pull" | not pull]
        , ["."]
        ]
  readProcess_ p

writeYaml :: (MonadIO m, ToJSON a) => FilePath -> a -> m ()
writeYaml path =
  liftIO
    . T.writeFile path
    . decodeUtf8With lenientDecode
    . Yaml.encode

testManifest :: FilePath
testManifest = "./manifest.yaml"
