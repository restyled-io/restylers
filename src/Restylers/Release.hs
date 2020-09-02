module Restylers.Release
    ( releaseRestylerImage
    , dockerHubImageExists
    )
where

import RIO

import Restylers.Image
import Restylers.Info (RestylerInfo)
import RIO.Process
import RIO.Text (unpack)

releaseRestylerImage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerInfo
    -> RestylerImage
    -> m ()
releaseRestylerImage _info image =
    proc "docker" ["push", unpack $ unRestylerImage image] runProcess_
