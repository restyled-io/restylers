module Restylers.Options
    ( Options(..)
    , HasOptions(..)
    , parseOptions
    )
where

import RIO

import Restylers.CommitSHA
import Restylers.Name
import Restylers.Registry

data Options = Options
    { oRegistry :: Registry
    , oManifest :: FilePath
    , oCommitSHA :: CommitSHA
    , oName :: Maybe RestylerName
    , oDebug :: Bool
    }
    deriving Show

class HasOptions env where
    optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = pure Options
    { oRegistry = Registry "restyled"
    , oManifest = "restylers.yaml"
    , oCommitSHA = CommitSHA "8d2a6e854a51ed2f0139675538ff002a214837f1"
    , oName = Nothing -- Just $ RestylerName "black"
    , oDebug = True
    }
