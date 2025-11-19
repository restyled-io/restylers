module RenovateAudit.Options
  ( Options (..)
  , parseOptions
  ) where

import Restylers.Prelude

newtype Options = Options
  { cwd :: Maybe FilePath
  }

parseOptions :: IO Options
parseOptions = pure $ Options {cwd = Just "."}
