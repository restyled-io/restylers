module Restylers.Build
    ( buildRestylerImage
    , pullRestylerImage
    , pushRestylerImage
    ) where

import RIO hiding (to)

import Restylers.Image
import qualified Restylers.Info.Build as Build
import Restylers.Info.Resolved (ImageSource(..), RestylerInfo)
import qualified Restylers.Info.Resolved as Info
import Restylers.Options
import Restylers.Version
import qualified RIO.ByteString.Lazy as BSL
import RIO.Directory (doesFileExist)
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T

buildRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => NoCache
    -> RestylerInfo
    -> m RestylerImage
buildRestylerImage noCache info = do
    registry <- oRegistry <$> view optionsL
    case Info.imageSource info of
        Explicit image -> do
            logInfo $ "Pulling explicit image, " <> display image
            image <$ proc "docker" ["pull", unImage image] runProcess
        BuildVersionCmd name cmd options -> do
            tag <- oTag <$> view optionsL
            image <- Build.build noCache options
                $ mkRestylerImage registry name tag
            version <- dockerRunSh image cmd
            let versioned = mkRestylerImage registry name version
            writeFileUtf8 (Info.restylerVersionCache name) $ version <> "\n"
            logInfo $ "Tagging " <> display image <> " => " <> display versioned
            versioned <$ dockerTag image versioned
        BuildVersion name version options -> do
            Build.build noCache options
                $ mkRestylerImage registry name
                $ unRestylerVersion version

pullRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => RestylerInfo
    -> m RestylerImage
pullRestylerImage info = do
    registry <- oRegistry <$> view optionsL
    image <- case Info.imageSource info of
        Explicit image -> pure image
        BuildVersionCmd name _ _ -> do
            let cache = Info.restylerVersionCache name
            exists <- doesFileExist cache
            if exists
                then do
                    version <- T.strip <$> readFileUtf8 cache
                    pure $ mkRestylerImage registry name version
                else do
                    logWarn
                        $ "Restyler "
                        <> display info
                        <> " uses version_cmd, but "
                        <> fromString cache
                        <> " does not exist."
                        <> " Building now..."
                    buildRestylerImage (NoCache False) info
        BuildVersion name version _ -> do
            pure $ mkRestylerImage registry name $ unRestylerVersion version
    image <$ proc "docker" ["pull", unImage image] runProcess_

pushRestylerImage
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerImage
    -> m ()
pushRestylerImage image = proc "docker" ["push", unImage image] runProcess_

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

dockerTag
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => RestylerImage
    -> RestylerImage
    -> m ()
dockerTag from to = proc "docker" ["tag", unImage from, unImage to] runProcess_

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
