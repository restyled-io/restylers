module Restylers.App
    ( App
    , loadApp
    )
where

import RIO

import Restylers.Manifest (HasRestylerManifest(..), RestylerManifest)
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import RIO.Process

data App = App
    { appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
    , appOptions :: Options
    , appManifest :: RestylerManifest
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasProcessContext App where
    processContextL =
        lens appProcessContext $ \x y -> x { appProcessContext = y }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

instance HasRestylerManifest App where
    manifestL = lens appManifest $ \x y -> x { appManifest = y }

-- brittany-disable-next-binding

loadApp :: Options -> LogFunc -> IO App
loadApp opts lf = App lf
    <$> mkDefaultProcessContext
    <*> pure opts
    <*> Manifest.load (oManifest opts)
