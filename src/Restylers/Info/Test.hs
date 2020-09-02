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
import Restylers.Info.Test.Support (Support, writeSupportFile)
import Restylers.Name
import RIO.Text (unpack)

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
