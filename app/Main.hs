module Main
    ( main
    )
where

import RIO

import Restylers.App
import Restylers.Build
import Restylers.CommitSHA
import Restylers.Image
import Restylers.Info (RestylerInfo, restylerInfoYaml)
import qualified Restylers.Info as Info
import qualified Restylers.Manifest as Manifest
import Restylers.Name
import Restylers.Options
import Restylers.Registry
import Restylers.Release
import Restylers.Test
import qualified RIO.Text as T
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
                Release -> do
                    yamls <- liftIO $ globDir1 "*/info.yaml" "."
                    updates <- fmap concat $ for yamls $ \yaml -> do
                        logDebug $ "Reading " <> fromString yaml
                        info <- Info.load yaml
                        let image = Info.image info oRegistry

                        -- This bit is coupled to DockerHub for now
                        exists <- do
                            when (unRegistry oRegistry /= "restyled")
                                $ throwString "TODO: Explain this"
                            dockerHubImageExists image

                        if exists
                            then do
                                logInfo
                                    $ "Skipping "
                                    <> display (Info.name info)
                                    <> ", "
                                    <> display image
                                    <> " exists"
                                pure []
                            else do
                                logInfo
                                    $ "Releasing "
                                    <> display (Info.name info)
                                    <> " to "
                                    <> display image
                                buildRestylerImage info image
                                testRestylerImage info image
                                releaseRestylerImage info image
                                pure [info]

                    logInfo "Re-writing manifest"
                    Manifest.writeUpdated oManifest updates

                Build names sha -> do
                    for_ names $ \name -> do
                        info <- Info.load $ restylerInfoYaml name
                        let image = buildImage oRegistry info sha
                        logDebug
                            $ "Building and testing "
                            <> display name
                            <> " as "
                            <> display image
                        buildRestylerImage info image
                        testRestylerImage info image

buildImage :: Registry -> RestylerInfo -> CommitSHA -> RestylerImage
buildImage registry info sha =
    RestylerImage
        $ unRegistry registry
        <> "/restyler-"
        <> unRestylerName (Info.name info)
        <> ":"
        <> T.take 7 (unCommitSHA sha)
