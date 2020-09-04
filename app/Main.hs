module Main
    ( main
    )
where

import RIO

import Data.Yaml as Yaml
import Restylers.App
import Restylers.Build
import Restylers.Options
import Restylers.Release
import Restylers.Restyler (Restyler)
import Restylers.Test

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
                Build noCache test yamls -> for_ yamls $ \yaml -> do
                    buildRestylerImage noCache yaml
                    when test $ testRestylerImage yaml
                Test yamls -> traverse_ testRestylerImage yamls
                Release manifest yamls -> do
                    restylers <- traverse releaseRestylerImage yamls
                    logInfo
                        $ "Writing "
                        <> displayShow (length restylers)
                        <> " Restyler(s) to "
                        <> fromString manifest
                    liftIO $ Yaml.encodeFile manifest restylers
