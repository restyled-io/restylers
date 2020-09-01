module Restylers.Build
    ( buildRestylerImage
    )
where

import RIO

import Restylers.Image
import Restylers.Info (RestylerInfo)
import qualified Restylers.Info as Info
import Restylers.Manifest (HasRestylerManifest(..))
import qualified Restylers.Manifest as Manifest
import Restylers.Name
import RIO.Process
import RIO.Text (unpack)

buildRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasRestylerManifest env
       )
    => RestylerInfo
    -> RestylerImage
    -> m ()
buildRestylerImage info image = do
    manifest <- view manifestL

    for_ (Manifest.lookup (Info.name info) manifest) $ \restyler -> do
        logInfo "Pulling currently-released image"
        proc "docker" ["pull", unImage $ Manifest.image restyler] runProcess_

    logInfo $ "Building updated image as " <> display image
    proc "docker" ["build", "--tag", unImage image, buildPath info] runProcess_
    where unImage = unpack . unRestylerImage

buildPath :: RestylerInfo -> FilePath
buildPath = unpack . unRestylerName . Info.name
