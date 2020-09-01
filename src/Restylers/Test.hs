module Restylers.Test
    ( testRestylerImage
    )
where

import RIO

import Restylers.Image
import Restylers.Info (RestylerInfo, Test)
import qualified Restylers.Info as Info
import Restylers.Name
import Restylers.Options
import RIO.Directory (getCurrentDirectory, withCurrentDirectory)
import RIO.List (nub)
import RIO.Process
import RIO.Text (unpack)
import Test.Hspec

testRestylerImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> m ()
testRestylerImage info = do
    let name = unpack $ unRestylerName $ Info.name info
        tests = numberTests $ Info.tests $ Info.metadata info

    inTempDir $ do
        logInfo $ "Setting up " <> fromString name <> " test cases"
        runRestyler info tests

        logInfo $ "Running " <> displayShow (length tests) <> " assertion(s)"
        liftIO $ hspec $ describe name $ for_ tests $ \nt ->
            it (nameTest nt) $ do
                updated <- readFileUtf8 $ testPath info nt
                updated `shouldBe` Info.restyled (ntTest nt)

inTempDir :: MonadUnliftIO m => m a -> m a
inTempDir f = withSystemTempDirectory "restylers-tests"
    $ \tmp -> withCurrentDirectory tmp f

runRestyler
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> [NumberedTest]
    -> m ()
runRestyler info tests = do
    for_ tests $ \nt ->
        writeFileUtf8 (testPath info nt) $ Info.contents $ ntTest nt

    cwd <- getCurrentDirectory
    registry <- oRegistry <$> view optionsL

    if Info.supports_multiple_paths info
        then dockerRun cwd registry relativePaths
        else traverse_ (dockerRun cwd registry . pure) relativePaths
  where
    -- Restyler prepends ./, so we do too
    relativePaths = map (("./" <>) . testPath info) tests

    -- Restyler uniques the created arguments, so we do too
    dockerRun cwd registry paths = proc
        "docker"
        (nub $ concat
            [ ["run", "--interactive", "--rm"]
            , ["--net", "none"]
            , ["--volume", cwd <> ":/code"]
            , [unpack $ unRestylerImage $ Info.image info registry]
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

numberTests :: [Test] -> [NumberedTest]
numberTests = zipWith NumberedTest [1 ..]

nameTest :: NumberedTest -> String
nameTest NumberedTest {..} = "Test case: " <> show ntNumber

testPath :: RestylerInfo -> NumberedTest -> FilePath
testPath info NumberedTest {..} =
    unpack
        $ unRestylerName (Info.name info)
        <> "-test-"
        <> tshow ntNumber
        <> "."
        <> fromMaybe "tmp" (Info.extension ntTest)
