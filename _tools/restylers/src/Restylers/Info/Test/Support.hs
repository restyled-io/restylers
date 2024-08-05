{-# LANGUAGE NamedFieldPuns #-}

module Restylers.Info.Test.Support
  ( Support (..)
  , writeSupportFile
  )
where

import RIO

import Data.Aeson

data Support = Support
  { path :: FilePath
  , contents :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

writeSupportFile
  :: ( MonadIO m
     , MonadReader env m
     , HasLogFunc env
     )
  => Support
  -> m ()
writeSupportFile Support {path, contents} = do
  logInfo $ "CREATE " <> fromString path
  writeFileUtf8 path contents
