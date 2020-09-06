module Restylers.Test
    ( testRestylerImage
    )
where

import RIO

import Restylers.Image
import qualified Restylers.Info.Metadata as Metadata
import Restylers.Info.Resolved (RestylerInfo)
import qualified Restylers.Info.Resolved as Info
import Restylers.Info.Test
    ( ExpectationFailure
    , Test
    , assertTestRestyled
    , testFilePath
    , writeTestFiles
    )
import Restylers.Options
import RIO.Directory (getCurrentDirectory, withCurrentDirectory)
import RIO.List (nub)
import RIO.Process
import RIO.Text (unpack)

testRestylerImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> RestylerImage
    -> m ()
testRestylerImage info image = do
    let tests = zip [1 ..] $ Metadata.tests $ Info.metadata info

    logInfo
        $ "Running "
        <> displayShow (length tests)
        <> " test(s) for "
        <> display info
        <> " with "
        <> display image

    inTempDir $ do
        runRestyler info image tests

        for_ tests $ \(number, test) -> do
            eResult <- try $ assertTestRestyled number (Info.name info) test
            either
                (\ex -> do
                    logError "Failed"
                    logError $ display @ExpectationFailure ex
                    exitFailure
                )
                (\_ -> logInfo "Passed")
                eResult

inTempDir :: MonadUnliftIO m => m a -> m a
inTempDir f = withSystemTempDirectory "restylers-tests"
    $ \tmp -> withCurrentDirectory tmp f

runRestyler
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerInfo
    -> RestylerImage
    -> [(Int, Test)]
    -> m ()
runRestyler info image tests = do
    for_ tests $ \(number, test) -> do
        writeTestFiles number (Info.name info) test

    cwd <- getCurrentDirectory

    if Info.supports_multiple_paths info
        then dockerRun cwd relativePaths
        else traverse_ (dockerRun cwd . pure) relativePaths
  where
    -- Restyler prepends ./, so we do too
    relativePaths = map
        (\(number, test) -> "./" <> testFilePath number (Info.name info) test)
        tests

    -- Restyler uniques the created arguments, so we do too
    dockerRun cwd paths = proc
        "docker"
        (nub $ concat
            [ ["run", "--interactive", "--rm"]
            , ["--net", "none"]
            , ["--volume", cwd <> ":/code"]
            , [unpack $ unRestylerImage image]
            , map unpack $ Info.command info
            , map unpack $ Info.arguments info
            , [ "--" | Info.supports_arg_sep info ]
            , paths
            ]
        )
        runProcess_
