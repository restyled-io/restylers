module Main
    ( main
    ) where

import RIO

import Restylers.App
import Restylers.Build
import qualified Restylers.Info.Resolved as Info
import Restylers.Lint
import Restylers.Manifest (toRestyler)
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import Restylers.Test

main :: IO ()
main = do
    opts@Options {..} <- parseOptions
    logOptions <- logOptionsHandle stdout oDebug
    withLogFunc logOptions $ \lf -> do
        app <- loadApp opts lf
        runRIO app $ do
            logDebug $ "Options: " <> displayShow opts
            case oCommand of
                Build noCache lint test push yaml -> do
                    info <- Info.load yaml
                    whenLintDockerfile lint $ lintRestyler info
                    image <- buildRestylerImage noCache info
                    whenRunTests test $ testRestylerImage info image
                    whenPush push $ pushRestylerImage image

                Release manifest yamls -> do
                    restylers <- for yamls $ \yaml -> do
                        info <- Info.load yaml
                        image <- pullRestylerImage info `catchAny` \ex -> do
                            logWarn $ display ex
                            logInfo "Attempting build and re-pull..."
                            image <- buildRestylerImage (NoCache False) info
                            testRestylerImage info image
                            pushRestylerImage image
                            pullRestylerImage info
                        pure $ toRestyler info image
                    logInfo
                        $ "Writing "
                        <> displayShow (length restylers)
                        <> " Restyler(s) to "
                        <> fromString manifest
                    liftIO $ Manifest.write manifest restylers
