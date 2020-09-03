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

main :: IO ()
main = do
    opts@Options {..} <- parseOptions
    logOptions <- logOptionsHandle stdout oDebug
    withLogFunc logOptions $ \lf -> do
        app <- loadApp opts lf
        runRIO app $ do
            logDebug $ "Options: " <> displayShow opts

            case oCommand of
                Build yamls -> do
                    restylers <- loadRestylers oRegistry yamls
                    traverse_ buildRestylerImage restylers
                Test yamls -> do
                    restylers <- loadRestylers oRegistry yamls
                    traverse_ testRestylerImage restylers
                Release build test yamls -> do
                    restylers <- loadRestylers oRegistry yamls

                    for_ restylers $ \restyler -> do
                        exists <- dockerHubImageExists $ Restyler.image restyler

                        if exists
                            then logInfo
                                ("Skipping "
                                <> display (Restyler.name restyler)
                                <> ", "
                                <> display (Restyler.image restyler)
                                <> " exists"
                                )
                            else do
                                when build $ buildRestylerImage restyler
                                when test $ testRestylerImage restyler
                                releaseRestylerImage restyler

                    logInfo "Updating manifest"
                    Manifest.writeUpdated oManifest $ NE.toList restylers

loadRestylers
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasRestylerManifest env
       , Traversable t
       )
    => Maybe Registry
    -> t FilePath
    -> m (t Restyler)
loadRestylers registry yamls = for yamls $ \yaml -> do
    logDebug $ "Reading " <> fromString yaml
    restyler <- Restyler.loadInfo registry yaml
    restyler <$ logDebug
        ("Processing " <> display (Restyler.name restyler) <> " as " <> display
            (Restyler.image restyler)
        )
