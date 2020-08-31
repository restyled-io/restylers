module Main
    ( main
    )
where

import RIO

import Restylers.App
import Restylers.Build
import qualified Restylers.Info as Info
import Restylers.Options
import RIO.Process

main :: IO ()
main = do
    opts@Options {..} <- parseOptions
    logOptions <- logOptionsHandle stdout oDebug
    withLogFunc logOptions $ \lf -> do
        app <- loadApp opts lf
        runRIO app $ do
            logDebug $ "Options: " <> displayShow opts

            info <- Info.load "black/info.yaml"
            buildRestylerImage info
