{-# LANGUAGE RecordWildCards #-}

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
  opts@Options {..} <- parseOptions

  runAppT opts $ do
    logDebug $ (:# []) $ "Options: " <> pack (show opts)
    restylers <- for oInput $ \path -> do
      yaml <- locateYaml path
      info <- Info.load yaml
      when oBuild $ buildRestylerImage info
      image <- tagRestylerImage info
      pure $ toRestyler info image

    testRestylers oPull restylers $ fromMaybe [] oHspecArgs

    when oPush $ for_ restylers $ \restyler -> do
      exists <- doesRestylerImageExist $ Manifest.image restyler
      if exists
        then logWarn "Not pushing, image exists"
        else do
          pushRestylerImage $ Manifest.image restyler

          traverse_ (traverse_ pushRestylerImage)
            $ getSeriesImages
            $ Manifest.image restyler

    traverse_ (liftIO . (`Manifest.write` restylers)) oWrite

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
