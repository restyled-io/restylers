module Restylers.Test
    ( testRestylerImage
    )
where

import RIO

import Restylers.Image
import Restylers.Info (RestylerInfo, Test)
import qualified Restylers.Info as Info
import Restylers.Name
import RIO.Directory (getCurrentDirectory, withCurrentDirectory)
import RIO.List (nub)
import RIO.Process
import RIO.Text (unpack)

data ExpectationFailure = ExpectationFailure
    { efName :: RestylerName
    , efTest :: NumberedTest
    , efActual :: Text
    }
    deriving stock Show
    deriving anyclass Exception

testRestylerImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => RestylerInfo
    -> RestylerImage
    -> m ()
testRestylerImage info image = do
    let name = unpack $ unRestylerName $ Info.name info
        tests = numberTests $ Info.tests $ Info.metadata info

    inTempDir $ do
        logInfo $ "Setting up " <> fromString name <> " test cases"
        runRestyler info image tests

        logInfo $ "Running " <> displayShow (length tests) <> " assertion(s)"
        for_ tests $ \nt -> do
            actual <- readFileUtf8 $ testPath info nt
            when (actual /= Info.restyled (ntTest nt))
                $ throwIO
                $ ExpectationFailure
                      { efName = Info.name info
                      , efTest = nt
                      , efActual = actual
                      }
            logInfo "Passed"

inTempDir :: MonadUnliftIO m => m a -> m a
inTempDir f = withSystemTempDirectory "restylers-tests"
    $ \tmp -> withCurrentDirectory tmp f

runRestyler
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerInfo
    -> RestylerImage
    -> [NumberedTest]
    -> m ()
runRestyler info image tests = do
    for_ tests $ \nt ->
        writeFileUtf8 (testPath info nt) $ Info.contents $ ntTest nt

    cwd <- getCurrentDirectory

    if Info.supports_multiple_paths info
        then dockerRun cwd relativePaths
        else traverse_ (dockerRun cwd . pure) relativePaths
  where
    -- Restyler prepends ./, so we do too
    relativePaths = map (("./" <>) . testPath info) tests

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

data NumberedTest = NumberedTest
    { ntNumber :: Int
    , ntTest :: Test
    }
    deriving stock Show

numberTests :: [Test] -> [NumberedTest]
numberTests = zipWith NumberedTest [1 ..]

testPath :: RestylerInfo -> NumberedTest -> FilePath
testPath info NumberedTest {..} =
    unpack
        $ unRestylerName (Info.name info)
        <> "-test-"
        <> tshow ntNumber
        <> "."
        <> fromMaybe "tmp" (Info.extension ntTest)
