{-# LANGUAGE NamedFieldPuns #-}

module Restylers.Info.Test
    ( Test(..)
    , writeTestFiles
    , ExpectationFailure(..)
    , assertTestRestyled
    , testFilePath
    )
where

import RIO

import Data.Aeson
import Data.Algorithm.Diff (Diff, PolyDiff(..))
import qualified Data.Algorithm.Diff as Diff
import Restylers.Info.Test.Support (Support, writeSupportFile)
import Restylers.Name
import RIO.Text (unpack)
import qualified RIO.Text as T

data Test = Test
    { extension :: Maybe Text
    , contents :: Text
    , restyled :: Text
    , support :: Maybe Support
    }
    deriving stock (Show, Generic)
    deriving anyclass FromJSON

writeTestFiles :: MonadIO m => Int -> RestylerName -> Test -> m ()
writeTestFiles number name test@Test { contents, support } = do
    writeFileUtf8 (testFilePath number name test) contents
    traverse_ writeSupportFile support

data ExpectationFailure = ExpectationFailure
    { efName :: RestylerName
    , efTest :: Test
    , efActual :: Text
    }
    deriving stock Show
    deriving anyclass Exception

instance Display ExpectationFailure where
    display ExpectationFailure {..} =
        "Test case failed for "
            <> display efName
            <> ":\n"
            <> display (renderDiff (restyled efTest) efActual)
            <> "\n"

renderDiff :: Text -> Text -> Text
renderDiff a b =
    T.unlines $ map renderDiffLine $ Diff.getDiff (T.lines a) (T.lines b)

renderDiffLine :: Diff Text -> Text
renderDiffLine = \case
    First removed -> "- " <> removed
    Second added -> "+ " <> added
    Both same _ -> "  " <> same

assertTestRestyled :: MonadIO m => Int -> RestylerName -> Test -> m ()
assertTestRestyled number name test@Test { restyled } = do
    actual <- readFileUtf8 $ testFilePath number name test
    when (actual /= restyled) $ throwIO $ ExpectationFailure
        { efName = name
        , efTest = test
        , efActual = actual
        }

testFilePath :: Int -> RestylerName -> Test -> FilePath
testFilePath number name Test { extension } =
    unpack
        $ unRestylerName name
        <> "-test-"
        <> tshow number
        <> "."
        <> fromMaybe "tmp" extension
