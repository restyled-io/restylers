-- |
--
-- Module      : Main
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Main
  ( main
  ) where

import Restylers.Prelude

import Restylers.App
import Restylers.Build
import Restylers.Image (getSeriesImages)
import Restylers.Info.Resolved qualified as Info
import Restylers.Manifest (toRestyler)
import Restylers.Manifest qualified as Manifest
import Restylers.Options
import Restylers.Test
import System.Exit (exitFailure)
import System.FilePath (takeExtension, (<.>), (</>))
import UnliftIO.Directory (doesFileExist)

main :: IO ()
main = do
  opts <- parseOptions

  runAppT opts $ do
    logDebug $ (:# []) $ "Options: " <> pack (show opts)
    restylers <- for opts.input $ \path -> do
      yaml <- locateYaml path
      info <- Info.load yaml
      when opts.build $ buildRestylerImage info
      image <- tagRestylerImage info
      pure $ toRestyler info image

    testRestylers opts.pull restylers $ fromMaybe [] opts.hspecArgs

    when opts.push $ for_ restylers $ \restyler -> do
      exists <- doesRestylerImageExist restyler.image
      if exists
        then logWarn "Not pushing, image exists"
        else do
          pushRestylerImage restyler.image
          case getSeriesImages restyler.image of
            Nothing -> logWarn "Image is not semantically-versioned"
            Just is -> traverse_ pushRestylerImage is

    traverse_ (liftIO . (`Manifest.write` restylers)) opts.write

locateYaml
  :: (MonadIO m, MonadLogger m) => FilePath -> m FilePath
locateYaml input
  | takeExtension input `elem` [".yml", ".yaml"] = pure input
  | otherwise = do
      let input' = input </> "info" <.> "yaml"
      exists <- doesFileExist input'
      input' <$ unless exists err
 where
  err :: (MonadIO m, MonadLogger m) => m ()
  err = do
    logError
      $ "Invalid PATH input. Must be .yml, .yaml, or a directory containing an info.yaml"
      :# ["path" .= input]
    liftIO exitFailure
