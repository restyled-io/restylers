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
  , writeSupportFile
  )
where

import Restylers.Prelude

import Data.Aeson
import Data.Text.IO qualified as T

data Support = Support
  { path :: FilePath
  , contents :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

writeSupportFile
  :: ( MonadIO m
     , MonadLogger m
     )
  => Support
  -> m ()
writeSupportFile Support {path, contents} = do
  logInfo $ "CREATE" :# ["path" .= path]
  liftIO $ T.writeFile path contents
