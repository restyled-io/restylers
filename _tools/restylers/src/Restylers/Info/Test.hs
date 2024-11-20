{-# LANGUAGE NamedFieldPuns #-}

-- |
--
-- Module      : Restylers.Info.Test
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Info.Test
  ( Test (..)
  , writeTestFiles
  , testFilePath
  , testDescription
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Restylers.Info.Test.Support (Support)
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
     , MonadLogger m
     )
  => Int
  -> RestylerName
  -> [Text]
  -> Test
  -> m FilePath
writeTestFiles number name include test@Test {contents} = do
  logInfo $ "CREATE" :# ["path" .= path]
  path <$ liftIO (T.writeFile path contents)
 where
  path = testFilePath number name include test

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
