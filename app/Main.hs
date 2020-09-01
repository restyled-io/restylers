module Main
    ( main
    )
where

import RIO

import Restylers.App
import Restylers.Build
import Restylers.Info (restylerInfoYaml)
import qualified Restylers.Info as Info
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import Restylers.Release
import Restylers.Test
import System.FilePath.Glob (globDir1)

main :: IO ()
main = do
    opts@Options {..} <- parseOptions
    logOptions <- logOptionsHandle stdout oDebug
    withLogFunc logOptions $ \lf -> do
        app <- loadApp opts lf
        runRIO app $ do
            logDebug $ "Options: " <> displayShow opts

            case oName of
                Nothing -> do
                    yamls <- liftIO $ globDir1 "*/info.yaml" "."
                    infos <- fmap concat $ for yamls $ \yaml -> do
                        logDebug $ "Releasing from " <> fromString yaml
                        info <- Info.load yaml
                        exists <- releaseRestylerImageExists info

                        if exists
                            then do
                                logInfo
                                    $ "Skipping "
                                    <> fromString yaml
                                    <> ", released imaged exists"
                                pure []
                            else do
                                buildRestylerImage info
                                testRestylerImage info
                                releaseRestylerImage info
                                pure [info]

                    unless (null infos) $ do
                        logInfo "Re-writing manifest"
                        Manifest.writeUpdated oManifest infos

                Just name -> do
                    logDebug $ "Building and testing " <> display name
                    info <- Info.load $ restylerInfoYaml name
                    buildRestylerImage info
                    testRestylerImage info
