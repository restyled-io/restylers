module Restylers.Build
    ( buildRestylerImage
    , buildTag
    )
where

import RIO

import Restylers.CommitSHA
import Restylers.Image
import Restylers.Info (RestylerInfo)
import qualified Restylers.Info as Info
import Restylers.Manifest (HasRestylerManifest(..))
import qualified Restylers.Manifest as Manifest
import Restylers.Name
import Restylers.Options
import Restylers.Registry
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T

buildRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRestylerManifest env
       )
    => RestylerInfo
    -> m ()
buildRestylerImage info = do
    Options {..} <- view optionsL
    manifest <- view manifestL

    for_ (Manifest.lookup (Info.name info) manifest) $ \restyler -> do
        logInfo "Pulling currently-released image"
        proc
            "docker"
            ["pull", unpack $ unRestylerImage $ Manifest.image restyler]
            runProcess_

    let tag = buildTag oRegistry info oCommitSHA
    logInfo $ "Building updated image as " <> displayShow tag
    proc "docker" ["build", "--tag", tag, buildPath info] runProcess_

buildTag :: Registry -> RestylerInfo -> CommitSHA -> String
buildTag registry info sha =
    unpack
        $ unRegistry registry
        <> "/restyler-"
        <> unRestylerName (Info.name info)
        <> ":"
        <> T.take 7 (unCommitSHA sha)

buildPath :: RestylerInfo -> FilePath
buildPath = unpack . unRestylerName . Info.name
