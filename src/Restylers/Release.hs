module Restylers.Release
    ( releaseRestylerImage
    , tagRestylerImage
    )
where

import RIO

import Data.Semigroup (getLast)
import Restylers.Build (buildRestylerImage)
import Restylers.Image
import Restylers.Info (RestylerInfo)
import qualified Restylers.Info as Info
import Restylers.Manifest (HasRestylerManifest)
import Restylers.Name
import Restylers.Options
import Restylers.Restyler (Restyler, loadRestylerInfo, mkDevImage, mkRestyler)
import Restylers.Test (testRestylerImage)
import Restylers.Version
import qualified RIO.ByteString.Lazy as BSL
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T

-- | Produce a versioned 'Restyler' as you would store in the 'RestylerManifest'
tagRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => FilePath
    -> m Restyler
tagRestylerImage yaml = do
    (info, image) <- loadRestylerInfo yaml $ \info -> do
        devImage <- mkDevImage info
        mkReleaseImage info devImage
    pure $ mkRestyler info image

-- | Promote a development tagged image to the release version
--
-- Skips any already-released tags, and builds/tests before pushing what isn't.
--
releaseRestylerImage
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRestylerManifest env
       )
    => FilePath
    -> m Restyler
releaseRestylerImage yaml = do
    (info, (devImage, releaseImage)) <- loadRestylerInfo yaml $ \info -> do
        devImage <- mkDevImage info
        releaseImage <- mkReleaseImage info devImage
        pure (devImage, releaseImage)

    let name = getLast $ Info.name info
    released <- dockerHubImageExists releaseImage

    if released
        then skipRelease name releaseImage
        else do
            buildRestylerImage False yaml
            testRestylerImage yaml
            promoteRelease name devImage releaseImage

    pure $ mkRestyler info releaseImage

skipRelease
    :: (MonadIO m, MonadReader env m, HasLogFunc env)
    => RestylerName
    -> RestylerImage
    -> m ()
skipRelease name releaseImage =
    logInfo
        $ "Skipping "
        <> display name
        <> ", "
        <> display releaseImage
        <> " exists"

promoteRelease
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerName
    -> RestylerImage
    -> RestylerImage
    -> m ()
promoteRelease name devImage releaseImage = do
    logInfo
        $ "Promoting "
        <> display name
        <> ", "
        <> display devImage
        <> " => "
        <> display releaseImage
    proc "docker" ["tag", unImage devImage, unImage releaseImage] runProcess_
    proc "docker" ["push", unImage releaseImage] runProcess_
  where
    unImage :: RestylerImage -> String
    unImage = unpack . unRestylerImage

mkReleaseImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> RestylerImage
    -> m RestylerImage
mkReleaseImage info devImage = do
    let releaseImageInputs =
            ( getLast $ Info.name info
            , getLast <$> Info.version_cmd info
            , getLast <$> Info.version info
            , getLast <$> Info.image info
            )

        logged method getImage = do
            image <- getImage
            image <$ logDebug
                ("Release image for "
                <> display (getLast $ Info.name info)
                <> " determined "
                <> method
                )

    registry <- oRegistry <$> view optionsL
    case releaseImageInputs of
        (_, _, _, Just image) -> logged "explicitly " $ pure image
        (name, _, Just version, _) ->
            logged ("by version, " <> display version)
                $ pure
                $ mkRestylerImage registry name
                $ unRestylerVersion version
        (name, Just versionCmd, _, _) -> do
            version <- dockerRunSh devImage versionCmd
            logged
                    ("by version_cmd, "
                    <> displayShow versionCmd
                    <> " => "
                    <> display version
                    )
                $ pure
                $ mkRestylerImage registry name version
        (_name, Nothing, Nothing, Nothing) -> undefined -- TODO

dockerRunSh
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerImage
    -> String
    -> m Text
dockerRunSh image cmd = do
    bs <- proc
        "docker"
        (concat
            [ ["run", "--rm"]
            , ["--entrypoint", "sh"]
            , [unpack $ unRestylerImage image]
            , ["-c", cmd]
            ]
        )
        readProcessStdout_
    pure $ T.strip $ decodeUtf8With lenientDecode $ BSL.toStrict bs
