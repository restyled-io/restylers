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
                Build noCache yamls ->
                    traverse_ (buildRestylerImage noCache) yamls
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

                logInfo "Cleaning up manifest..."
                cleanupManifest manifest
            else do
                traverse_ (logError . display) errors
                logError "Use --write to store this change"
                exitFailure

cleanupManifest
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => FilePath
    -> m ()
cleanupManifest manifest = do
    proc "./build/sort-yaml" (manifest : keys) runProcess_
    proc "./build/restyle-path" [manifest] runProcess_
  where
    keys =
        [ "enabled"
        , "name"
        , "image"
        , "command"
        , "arguments"
        , "include"
        , "interpreters"
        , "supports_arg_sep"
        , "supports_multiple_paths"
        , "documentation"
        ]
