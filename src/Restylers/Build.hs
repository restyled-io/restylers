module Restylers.Build
    ( buildRestylerImage
    )
where

import RIO

import Restylers.Image
import Restylers.Info (restylerInfoYaml)
import Restylers.Manifest (HasRestylerManifest(..))
import qualified Restylers.Manifest as Manifest
import Restylers.Restyler (Restyler)
import qualified Restylers.Restyler as Restyler
import RIO.FilePath (takeDirectory)
import RIO.Process
import RIO.Text (unpack)

buildRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasRestylerManifest env
       )
    => Restyler
    -> m ()
buildRestylerImage restyler = do
    manifest <- view manifestL

    for_ (Manifest.lookup (Restyler.name restyler) manifest) $ \released -> do
        logInfo "Pulling currently-released image"
        proc "docker" ["pull", unImage $ Restyler.image released] runProcess_

    logInfo $ "Building updated image as " <> display image
    proc
        "docker"
        ["build", "--tag", unImage image, buildPath restyler]
        runProcess_
    where image = Restyler.image restyler

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage

buildPath :: Restyler -> FilePath
buildPath = takeDirectory . restylerInfoYaml . Restyler.name
