{-# LANGUAGE NamedFieldPuns #-}

-- |
--
-- Module      : Restylers.Info.Test
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Info.Test
  ( Test (..)
  , writeTestFile
  , writeTestSupportFile
  , testDescription
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Restylers.Info.Test.Support (Support (..))
import Restylers.Name
import System.FilePath (takeExtension)

data Test = Test
  { name :: Maybe Text
  , extension :: Maybe Text
  , contents :: Text
  , restyled :: Text
  , support :: Maybe Support
  , command :: Maybe [Text]
  , arguments :: Maybe [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

writeTestFile
  :: MonadIO m
  => Int
  -> RestylerName
  -> [Text]
  -> Test
  -> m FilePath
writeTestFile number name include test@Test {contents} = do
  path <$ liftIO (T.writeFile path contents)
 where
  path = testFilePath number name include test

writeTestSupportFile :: MonadIO m => Test -> m ()
writeTestSupportFile Test {support} = do
  for_ support $ \Support {path, contents} ->
    liftIO $ T.writeFile path contents

testFilePath :: Int -> RestylerName -> [Text] -> Test -> FilePath
testFilePath number name include Test {extension} =
  unpack name.unwrap
    <> "-test-"
    <> show number
    <> maybe (guessExtension include) (("." <>) . unpack) extension

guessExtension :: [Text] -> String
guessExtension =
  maybe ".tmp" (takeExtension . unpack . NE.head)
    . NE.nonEmpty
    . filter ("." `T.isInfixOf`)
    . filter (not . ("!" `T.isPrefixOf`))

testDescription :: Int -> Test -> String
testDescription number Test {name} =
  unpack
    $ fromMaybe
      ("Test #" <> pack (show number))
      name
