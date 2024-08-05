{-# LANGUAGE NamedFieldPuns #-}

module Restylers.Info.Test
  ( Test (..)
  , writeTestFiles
  , testFilePath
  , testDescription
  )
where

import RIO

import Data.Aeson
import RIO.List (headMaybe)
import RIO.Text (pack, unpack)
import qualified RIO.Text as T
import Restylers.Info.Test.Support (Support, writeSupportFile)
import Restylers.Name
import System.FilePath (takeExtension)

data Test = Test
  { name :: Maybe Text
  , extension :: Maybe Text
  , contents :: Text
  , restyled :: Text
  , support :: Maybe Support
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

writeTestFiles
  :: ( MonadIO m
     , MonadReader env m
     , HasLogFunc env
     )
  => Int
  -> RestylerName
  -> [Text]
  -> Test
  -> m ()
writeTestFiles number name include test@Test {contents, support} = do
  logInfo $ "CREATE " <> fromString path
  writeFileUtf8 path contents
  traverse_ writeSupportFile support
 where
  path = testFilePath number name include test

testFilePath :: Int -> RestylerName -> [Text] -> Test -> FilePath
testFilePath number name include Test {extension} =
  unpack (unRestylerName name)
    <> "-test-"
    <> show number
    <> maybe (guessExtension include) (("." <>) . unpack) extension

guessExtension :: [Text] -> String
guessExtension =
  maybe ".tmp" (takeExtension . unpack)
    . headMaybe
    . filter ("." `T.isInfixOf`)
    . filter (not . ("!" `T.isPrefixOf`))

testDescription :: Int -> Test -> String
testDescription number Test {name} =
  unpack
    $ fromMaybe
      ("Test #" <> pack (show number))
      name
