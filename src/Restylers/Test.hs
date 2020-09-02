module Restylers.Test
    ( testRestylerImage
    )
where

import RIO

import Restylers.Image
import Restylers.Info (restylerInfoYaml)
import qualified Restylers.Info.Metadata as Metadata
import Restylers.Info.Test
    (Test, assertTestRestyled, testFilePath, writeTestFiles)
import Restylers.Name
import Restylers.Restyler (Restyler)
import qualified Restylers.Restyler as Restyler
import RIO.Directory (getCurrentDirectory, withCurrentDirectory)
import RIO.List (nub)
import RIO.Process
import RIO.Text (unpack)

testRestylerImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => Restyler
    -> m ()
testRestylerImage restyler = do
    metadata <- Metadata.load $ restylerInfoYaml $ Restyler.name restyler

    let name = unpack $ unRestylerName $ Restyler.name restyler
        tests = zip [1 ..] $ Metadata.tests metadata

    inTempDir $ do
        logInfo $ "Setting up " <> fromString name <> " test cases"
        runRestyler restyler tests

        logInfo $ "Running " <> displayShow (length tests) <> " assertion(s)"
        for_ tests $ \(number, test) -> do
            assertTestRestyled number (Restyler.name restyler) test
            logInfo "Passed"

inTempDir :: MonadUnliftIO m => m a -> m a
inTempDir f = withSystemTempDirectory "restylers-tests"
    $ \tmp -> withCurrentDirectory tmp f

runRestyler
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Restyler
    -> [(Int, Test)]
    -> m ()
runRestyler restyler tests = do
    -- TODO: This might clobber support files between cases, so, heads-up
    for_ tests $ \(number, test) -> do
        writeTestFiles number (Restyler.name restyler) test

    cwd <- getCurrentDirectory

    if Restyler.supports_multiple_paths restyler
        then dockerRun cwd relativePaths
        else traverse_ (dockerRun cwd . pure) relativePaths
  where
    -- Restyler prepends ./, so we do too
    relativePaths = map
        (\(number, test) ->
            "./" <> testFilePath number (Restyler.name restyler) test
        )
        tests

    -- Restyler uniques the created arguments, so we do too
    dockerRun cwd paths = proc
        "docker"
        (nub $ concat
            [ ["run", "--interactive", "--rm"]
            , ["--net", "none"]
            , ["--volume", cwd <> ":/code"]
            , [unpack $ unRestylerImage $ Restyler.image restyler]
            , map unpack $ Restyler.command restyler
            , map unpack $ Restyler.arguments restyler
            , [ "--" | Restyler.supports_arg_sep restyler ]
            , paths
            ]
        )
        runProcess_
