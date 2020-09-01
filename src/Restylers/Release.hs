module Restylers.Release
    ( releaseRestylerImage
    , releaseRestylerImageExists
    )
where

import RIO

import Network.HTTP.Simple
import Network.HTTP.Types.Status (status200)
import Restylers.Build (buildTag)
import Restylers.Image
import Restylers.Info (RestylerInfo)
import qualified Restylers.Info as Info
import Restylers.Options
import Restylers.Registry
import RIO.Process
import RIO.Text (pack, unpack)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOn)

releaseRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> m ()
releaseRestylerImage info = do
    Options {..} <- view optionsL

    let built = buildTag oRegistry info oCommitSHA
        release = unpack $ unRestylerImage $ Info.image info oRegistry

    logInfo $ "Releasing " <> fromString built <> " => " <> fromString release
    proc "docker" ["tag", built, release] runProcess_
    --proc "docker" ["push", release] runProcess_

releaseRestylerImageExists
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasOptions env)
    => RestylerInfo
    -> m Bool
releaseRestylerImageExists info = do
    Options {..} <- view optionsL
    assertRestyledRegistry oRegistry
    uncurry dockerHubImageExists
        $ splitNameTag
        $ unpack
        $ unRestylerImage
        $ Info.image info oRegistry

assertRestyledRegistry :: MonadIO m => Registry -> m ()
assertRestyledRegistry registry =
    when (unRegistry registry /= "restyled") $ throwString "TODO: explain this"

splitNameTag :: String -> (String, String)
splitNameTag =
    bimap (unpack . ("restyled/" <>)) (unpack . T.drop 1)
        . T.breakOn ":"
        . T.takeWhileEnd (/= '/')
        . pack

dockerHubImageExists
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => String
    -> String
    -> m Bool
dockerHubImageExists name tag = do
    let base = "https://index.docker.io/v1/repositories"
        url = base <> "/" <> name <> "/tags/" <> tag
    logDebug $ "Checking " <> fromString url
    let req = parseRequest_ url
    resp <- httpNoBody $ setRequestIgnoreStatus req
    let status = getResponseStatus resp
    logDebug $ "Status " <> displayShow status
    pure $ status == status200
