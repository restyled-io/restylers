module Main
    ( main
    )
where

import RIO

import Restylers.App
import Restylers.Build
import Restylers.Image
import Restylers.Info (RestylerInfo)
import qualified Restylers.Info as Info
import Restylers.Manifest (HasRestylerManifest)
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import Restylers.Registry
import Restylers.Release
import Restylers.Test
import qualified RIO.NonEmpty as NE
import RIO.Process
import System.FilePath.Glob (globDir1)

main :: IO ()
main = do
    opts@Options {..} <- parseOptions
    logOptions <- logOptionsHandle stdout oDebug
    withLogFunc logOptions $ \lf -> do
        app <- loadApp opts lf
        runRIO app $ do
            logDebug $ "Options: " <> displayShow opts

            case oCommand of
                List -> do
                    yamls <- liftIO $ globDir1 "*/info.yaml" "."
                    void $ withEachInfoImage oRegistry yamls $ \info _ ->
                        logInfo $ display $ Info.name info
                Build yamls -> void
                    $ withEachInfoImage oRegistry yamls buildRestylerImage
                Test yamls ->
                    void $ withEachInfoImage oRegistry yamls testRestylerImage
                Release yamls -> do
                    updates <-
                        withEachInfoImage oRegistry yamls $ \info image -> do
                            exists <- dockerHubImageExists image

                            if exists
                                then [] <$ logInfo
                                    ("Skipping "
                                    <> display (Info.name info)
                                    <> ", "
                                    <> display image
                                    <> " exists"
                                    )
                                else [info] <$ releaseRestylerImage info image

                    logInfo "Re-writing manifest"
                    Manifest.writeUpdated oManifest $ concat $ NE.toList updates

withEachInfoImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasRestylerManifest env
       , Traversable t
       )
    => Maybe Registry
    -> t FilePath
    -> (RestylerInfo -> RestylerImage -> m a)
    -> m (t a)
withEachInfoImage registry yamls f = for yamls $ \yaml -> do
    logDebug $ "Reading " <> fromString yaml
    info <- Info.load yaml
    let image = Info.image info registry
    logDebug
        $ "Processing "
        <> display (Info.name info)
        <> " as "
        <> display image
    f info image
