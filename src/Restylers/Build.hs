module Restylers.Build
    ( buildRestylerImage
    , mkBuildPath
    )
where

import RIO

import Data.Semigroup (getLast)
import Restylers.Image
import Restylers.Info (RestylerInfo, restylerInfoYaml)
import qualified Restylers.Info as Info
import Restylers.Options
import Restylers.Restyler (loadRestylerInfo, mkDevImage)
import RIO.FilePath (takeDirectory)
import RIO.Process
import RIO.Text (unpack)

-- | [Re]build an image at the development tag
--
-- - Attempst to pull existing development tag
-- - Builds, tags and pushes at development tag
--
buildRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => Bool -- ^ No cache?
    -> FilePath
    -> m ()
buildRestylerImage noCache yaml = do
    (_, (image, buildPath)) <- loadRestylerInfo yaml
        $ \info -> (,) <$> mkDevImage info <*> pure (mkBuildPath info)

    logInfo $ "Pulling previous build image, " <> display image
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
