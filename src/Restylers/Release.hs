module Restylers.Release
    ( releaseRestylerImage
    , dockerHubImageExists
    )
where

import RIO

import Network.HTTP.Simple
import Network.HTTP.Types.Status (status200)
import Restylers.Image
import Restylers.Info (RestylerInfo)
import RIO.Process
import RIO.Text (pack, unpack)
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (breakOn)

releaseRestylerImage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerInfo
    -> RestylerImage
    -> m ()
releaseRestylerImage _info image =
    proc "docker" ["push", unpack $ unRestylerImage image] runProcess_

dockerHubImageExists
    :: (MonadIO m, MonadReader env m, HasLogFunc env) => RestylerImage -> m Bool
dockerHubImageExists image = do
    logDebug $ "Checking " <> fromString indexUrl
    let req = parseRequest_ indexUrl
    resp <- httpNoBody $ setRequestIgnoreStatus req
    let status = getResponseStatus resp
    logDebug $ "Status " <> displayShow status
    pure $ status == status200
  where
    indexUrl =
        let (name, tag) = splitNameTag $ unpack $ unRestylerImage image
        in "https://index.docker.io/v1/repositories/" <> name <> "/tags/" <> tag

splitNameTag :: String -> (String, String)
splitNameTag =
    bimap (unpack . ("restyled/" <>)) (unpack . T.drop 1)
        . T.breakOn ":"
        . T.takeWhileEnd (/= '/')
        . pack
