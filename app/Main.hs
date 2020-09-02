module Main
    ( main
    )
where

import RIO

import Restylers.App
import Restylers.Build
import Restylers.Image
import Restylers.Manifest (HasRestylerManifest)
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import Restylers.Registry
import Restylers.Release
import Restylers.Restyler (Restyler)
import qualified Restylers.Restyler as Restyler
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
                    void $ withEachRestyler oRegistry yamls $ \restyler ->
                        logInfo $ display $ Restyler.name restyler
                Build yamls -> void
                    $ withEachRestyler oRegistry yamls buildRestylerImage
                Test yamls ->
                    void $ withEachRestyler oRegistry yamls testRestylerImage
                Release yamls -> do
                    updates <- withEachRestyler oRegistry yamls $ \restyler ->
                        do
                            exists <- dockerHubImageExists
                                $ Restyler.image restyler

                            if exists
                                then [] <$ logInfo
                                    ("Skipping "
                                    <> display (Restyler.name restyler)
                                    <> ", "
                                    <> display (Restyler.image restyler)
                                    <> " exists"
                                    )
                                else [restyler] <$ releaseRestylerImage restyler

                    logInfo "Re-writing manifest"
                    Manifest.writeUpdated oManifest $ concat $ NE.toList updates

withEachRestyler
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasRestylerManifest env
       , Traversable t
       )
    => Maybe Registry
    -> t FilePath
    -> (Restyler -> m a)
    -> m (t a)
withEachRestyler registry yamls f = for yamls $ \yaml -> do
    logDebug $ "Reading " <> fromString yaml
    restyler <- Restyler.loadInfo registry yaml
    logDebug
        $ "Processing "
        <> display (Restyler.name restyler)
        <> " as "
        <> display (Restyler.image restyler)
    f restyler
