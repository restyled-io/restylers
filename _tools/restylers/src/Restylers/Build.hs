-- |
--
-- Module      : Restylers.Build
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Build
  ( buildRestylerImage
  , tagRestylerImage
  , doesRestylerImageExist
  , pushRestylerImage
  , dockerRunSh
  ) where

import Restylers.Prelude

import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as T
import Restylers.Image
import Restylers.Info.Build qualified as Build
import Restylers.Info.Resolved (ImageSource (..), RestylerInfo)
import Restylers.Info.Resolved qualified as Info
import Restylers.Options
import Restylers.Version
import System.Environment (getEnvironment)
import System.Exit (exitFailure)
import System.Process.Typed

buildRestylerImage
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasOptions env
     )
  => RestylerInfo
  -> m ()
buildRestylerImage info = do
  prefix <- asks $ (.prefix) . view optionsL
  sha <- asks $ (.sha) . view optionsL
  quiet <- asks $ not . (.debug) . view optionsL
  case info.imageSource of
    Explicit x -> do
      logInfo $ "Pulling explicit image" :# ["image" .= x]
      pullRestylerImage x
    BuildVersionCmd name _cmd options -> do
      image <- mkRestylerImageThrow prefix name sha
      logInfo $ "Building" :# ["image" .= image]
      void $ Build.build quiet options image
    BuildVersion name _version options -> do
      image <- mkRestylerImageThrow prefix name sha
      logInfo $ "Building" :# ["image" .= image]
      void $ Build.build quiet options image

tagRestylerImage
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     , HasOptions env
     )
  => RestylerInfo
  -> m RestylerImage
tagRestylerImage info = do
  prefix <- asks $ (.prefix) . view optionsL
  sha <- asks $ (.sha) . view optionsL

  let mkVersioned name getVersion = do
        image <- mkRestylerImageThrow prefix name sha
        version <- getVersion image

        when (T.null version) $ do
          logError "Unable to determine image version"
          liftIO exitFailure

        versioned <- mkRestylerImageThrow prefix name version
        versioned <$ dockerTag image versioned

  image <- case info.imageSource of
    Explicit image -> pure image
    BuildVersionCmd name cmd _ -> do
      logInfo $ "Running for version_cmd" :# ["name" .= name]
      mkVersioned name (`dockerRunSh` cmd)
    BuildVersion name explicitVersion _ -> do
      logInfo $ "Tagging as explicit version" :# ["name" .= name]
      mkVersioned name $ \image -> do
        pullRestylerImage image `catch` \ex ->
          logWarn
            $ "Error pulling, assuming local-only image"
            :# ["code" .= show (eceExitCode ex)]
        pure explicitVersion.unwrap

  logInfo $ "Tagged" :# ["image" .= image]

  for_ (getSeriesImages image) $ \seriesImages -> do
    for_ seriesImages $ \seriesImage -> do
      dockerTag image seriesImage
      logInfo $ "Tagged" :# ["image" .= seriesImage]

  pure image

doesRestylerImageExist
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => RestylerImage
  -> m Bool
doesRestylerImageExist image = do
  env <- liftIO $ (<> [("DOCKER_CLI_EXPERIMENTAL", "enabled")]) <$> getEnvironment
  (ec, _stdout, _stderr) <-
    readProcess
      . setEnv env
      =<< loggedProc "docker" ["manifest", "inspect", unImage image]
  pure $ ec == ExitSuccess

pullRestylerImage
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => RestylerImage
  -> m ()
pullRestylerImage image = do
  logInfo $ "Pulling" :# ["image" .= image]
  runProcess_ =<< loggedProc "docker" ["pull", unImage image]

pushRestylerImage
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => RestylerImage
  -> m ()
pushRestylerImage image = do
  logInfo $ "Pushing" :# ["image" .= image]
  runProcess_ =<< loggedProc "docker" ["push", unImage image]

dockerRunSh
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => RestylerImage
  -> String
  -> m Text
dockerRunSh image cmd = do
  p <-
    loggedProc "docker"
      $ concat
        [ ["run", "--rm"]
        , ["--entrypoint", "sh"]
        , [unpack $ unRestylerImage image]
        , ["-c", cmd]
        ]
  bs <- readProcessStdout_ p
  pure $ T.strip $ decodeUtf8With lenientDecode $ BSL.toStrict bs

dockerTag
  :: ( MonadIO m
     , MonadLogger m
     , MonadReader env m
     , HasLogger env
     )
  => RestylerImage
  -> RestylerImage
  -> m ()
dockerTag from to = runProcess_ =<< loggedProc "docker" ["tag", unImage from, unImage to]

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
