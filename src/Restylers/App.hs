module Restylers.App
    ( App
    , loadApp
    )
where

import RIO

import Restylers.Options
import RIO.Process

data App = App
    { appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
    , appOptions :: Options
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x { appLogFunc = y }

instance HasProcessContext App where
    processContextL =
        lens appProcessContext $ \x y -> x { appProcessContext = y }

instance HasOptions App where
    optionsL = lens appOptions $ \x y -> x { appOptions = y }

-- brittany-disable-next-binding

loadApp :: Options -> LogFunc -> IO App
loadApp opts lf = App lf
    <$> mkDefaultProcessContext
    <*> pure opts
