{-# LANGUAGE NamedFieldPuns #-}

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
