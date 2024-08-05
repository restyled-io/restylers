module Restylers.Build
  ( buildRestylerImage
  , tagRestylerImage
  , doesRestylerImageExist
  , pushRestylerImage
  , dockerRunSh
  ) where

import RIO hiding (to)

import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.Map as Map
import RIO.Process
import RIO.Text (unpack)
import qualified RIO.Text as T
import Restylers.Image
import qualified Restylers.Info.Build as Build
import Restylers.Info.Resolved (ImageSource (..), RestylerInfo)
import qualified Restylers.Info.Resolved as Info
import Restylers.Options
import Restylers.Version

buildRestylerImage
  :: ( MonadIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     , HasOptions env
     )
  => RestylerInfo
  -> m ()
buildRestylerImage info = do
  registry <- oRegistry <$> view optionsL
  sha <- oSha <$> view optionsL
  quiet <- not . oDebug <$> view optionsL
  case Info.imageSource info of
    Explicit x -> logInfo $ "Not bulding explicit image, " <> display x
    BuildVersionCmd name _cmd options -> do
      let image = mkRestylerImage registry name sha
      logInfo $ "Building " <> display image
      void $ Build.build quiet options image
    BuildVersion name _version options -> do
      let image = mkRestylerImage registry name sha
      logInfo $ "Building " <> display image
      void $ Build.build quiet options image

tagRestylerImage
  :: ( MonadUnliftIO m
     , MonadReader env m
     , HasLogFunc env
     , HasProcessContext env
     , HasOptions env
     )
  => RestylerInfo
  -> m RestylerImage
tagRestylerImage info = do
  registry <- oRegistry <$> view optionsL

  let mkVersioned name getVersion = do
        sha <- oSha <$> view optionsL
        let image = mkRestylerImage registry name sha
        version <- getVersion image
        let versioned = mkRestylerImage registry name version
        versioned <$ dockerTag image versioned

  image <- case Info.imageSource info of
    Explicit image -> pure image
    BuildVersionCmd name cmd _ -> do
      logInfo $ "Running " <> display name <> " for version_cmd"
      mkVersioned name (`dockerRunSh` cmd)
    BuildVersion name explicitVersion _ -> do
      logInfo $ "Tagging " <> display name <> " as explicit version"
      mkVersioned name $ \image -> do
        pullRestylerImage image `catch` \ex ->
          logWarn
            $ "Error pulling ("
            <> displayShow (eceExitCode ex)
            <> "), assuming local-only image"
        pure $ unRestylerVersion explicitVersion

  logInfo $ "Tagged " <> display image

  for_ (getSeriesImages image) $ \seriesImages -> do
    for_ seriesImages $ \seriesImage -> do
      dockerTag image seriesImage
      logInfo $ "Tagged " <> display seriesImage

  pure image

doesRestylerImageExist
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => RestylerImage
  -> m Bool
doesRestylerImageExist image =
  withModifyEnvVars (Map.insert "DOCKER_CLI_EXPERIMENTAL" "enabled") $ do
    (ec, _stdout, _stderr) <-
      proc
        "docker"
        ["manifest", "inspect", unImage image]
        readProcess
    pure $ ec == ExitSuccess

pullRestylerImage
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => RestylerImage
  -> m ()
pullRestylerImage image = do
  logInfo $ "Pulling " <> display image
  proc "docker" ["pull", unImage image] runProcess_

pushRestylerImage
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => RestylerImage
  -> m ()
pushRestylerImage image = do
  logInfo $ "Pushing " <> display image
  proc "docker" ["push", unImage image] runProcess_

dockerRunSh
  :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
  => RestylerImage
  -> String
  -> m Text
dockerRunSh image cmd = do
  bs <-
    proc
      "docker"
      ( concat
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
