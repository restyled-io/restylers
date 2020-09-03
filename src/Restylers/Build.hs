

module Restylers.Build
    ( buildRestylerImage
    )
where

import RIO

import Data.Semigroup (getLast)
import Restylers.Image
import Restylers.Info (RestylerInfo, restylerInfoYaml)
import qualified Restylers.Info as Info
import Restylers.Manifest (HasRestylerManifest(..))
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import Restylers.Restyler (loadRestylerInfo, mkDevImage)
import qualified Restylers.Restyler as Restyler
import RIO.FilePath (takeDirectory)
import RIO.Process
import RIO.Text (unpack)

-- | [Re]build an image at the development tag
--
-- - Pulls currenty-released tag to start, if known
-- - Attempst to pull existing development tag
-- - Builds, tags and pushes at development tag
--
buildRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRestylerManifest env
       )
    => Bool -- ^ No cache?
    -> FilePath
    -> m ()
buildRestylerImage noCache yaml = do
    (info, (image, buildPath)) <- loadRestylerInfo yaml
        $ \info -> (,) <$> mkDevImage info <*> pure (mkBuildPath info)

    mReleased <- Manifest.lookup $ getLast $ Info.name info
    for_ mReleased $ \released -> do
        logInfo "Pulling currently-released image"
        proc "docker" ["pull", unImage $ Restyler.image released] runProcess_

    logInfo "Pulling previous build image"
    void $ proc "docker" ["pull", unImage image] runProcess

    logInfo $ "Building updated image as " <> display image
    proc
        "docker"
        (concat
            [ ["build"]
            , ["--tag", unImage image]
            , [ "--no-cache" | noCache ]
            , [buildPath]
            ]
        )
        runProcess_

mkBuildPath :: RestylerInfo -> FilePath
mkBuildPath = takeDirectory . restylerInfoYaml . getLast . Info.name

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
