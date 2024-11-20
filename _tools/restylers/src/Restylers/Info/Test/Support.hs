{-# LANGUAGE NamedFieldPuns #-}

-- |
--
-- Module      : Restylers.Info.Test.Support
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Info.Test.Support
  ( Support (..)
  , withSupportFile
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.Text.IO qualified as T
import UnliftIO.Directory (removeFile)
import UnliftIO.Exception (finally)

data Support = Support
  { path :: FilePath
  , contents :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

withSupportFile
  :: MonadUnliftIO m
  => Support
  -> m a
  -> m a
withSupportFile Support {path, contents} f = do
  liftIO $ T.writeFile path contents
  f `finally` removeFile path
