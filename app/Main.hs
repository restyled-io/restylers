module Main
    ( main
    )
where

import RIO

import Restylers.App
import Restylers.Build
import Restylers.Check
import Restylers.Manifest (HasRestylerManifest(..))
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import Restylers.Release
import Restylers.Restyler (Restyler)
import Restylers.Test
import qualified RIO.NonEmpty as NE
import RIO.Process

data RestylersCheckError
    = UnexpectedRestyler Restyler
    | RestylerChangedFrom Restyler Restyler
    deriving stock Show
    deriving anyclass Exception

main :: IO ()
main = do
    opts@Options {..} <- parseOptions
    logOptions <- logOptionsHandle stdout oDebug
    withLogFunc logOptions $ \lf -> do
        app <- loadApp opts lf
        runRIO app $ do
            logDebug $ "Options: " <> displayShow opts

            case oCommand of
                Build yamls -> traverse_ buildRestylerImage yamls
                Test yamls -> traverse_ testRestylerImage yamls
                Check write yamls -> runCheck oManifest write yamls
                Release yamls -> traverse_ releaseRestylerImage yamls

runCheck
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRestylerManifest env
       )
    => FilePath
    -- ^ Manifest path
    -> Bool
    -- ^ Write?
    -> NonEmpty FilePath
    -> m ()
runCheck manifest write yamls = do
    results <- traverse checkRestylersImage yamls
    let (errors, _) = partitionEithers $ NE.toList results

    unless (null errors) $ do
        if write
            then do
                logInfo "Updating manifest..."
                Manifest.writeUpdated manifest
                    $ map restylersCheckErrorRestyler errors
            else do
                traverse_ (logError . display) errors
                logError
                    "If this is intentional, re-run check with the --write option to write this change to the manifest."
                exitFailure
