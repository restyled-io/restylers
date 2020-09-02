module Restylers.Release
    ( releaseRestylerImage
    )
where

import RIO

import Restylers.Image
import Restylers.Restyler (Restyler)
import qualified Restylers.Restyler as Restyler
import RIO.Process
import RIO.Text (unpack)

releaseRestylerImage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => Restyler
    -> m ()
releaseRestylerImage restyler = proc "docker" ["push", image] runProcess_
    where image = unpack $ unRestylerImage $ Restyler.image restyler
