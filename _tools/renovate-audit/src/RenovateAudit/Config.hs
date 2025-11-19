module RenovateAudit.Config
  ( findRenovatedConfigured
  ) where

import Restylers.Prelude

import Control.Applicative (asum)
import Control.Monad.Trans.Maybe (MaybeT (..), hoistMaybe, runMaybeT)
import Data.List (find)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import RenovateAudit.Dependencies (Dependency (..))
import RenovateAudit.Pretty
import System.Directory (doesFileExist)
import System.FilePath ((</>))

findRenovatedConfigured :: MonadIO m => Dependency -> m (Maybe (Doc Ann))
findRenovatedConfigured dependency =
  runMaybeT
    $ asum
    $ viaDockerfilePragma
    : viaDockerfileFrom
    : map viaManifest manifestPaths
 where
  viaDockerfilePragma :: MonadIO m => MaybeT m (Doc Ann)
  viaDockerfilePragma = do
    let dockerfile = unpack dependency.depName </> "Dockerfile"
    contents <- readFileExists dockerfile

    ln <- hoistMaybe $ getRenovatePragmaLine contents

    pure
      $ "configured by renovate pragma at"
      <+> annotate FilePath (pretty dockerfile)
      <> ":"
      <> annotate LineNumber (pretty ln)

  viaDockerfileFrom :: MonadIO m => MaybeT m (Doc Ann)
  viaDockerfileFrom = do
    let dockerfile = unpack dependency.depName </> "Dockerfile"
    contents <- readFileExists dockerfile

    guard
      $ or
        [ ("FROM " <> dependency.packageName <> ":") `T.isInfixOf` contents
        , ("from " <> dependency.packageName <> ":") `T.isInfixOf` contents
        ]

    pure
      $ "configured as base image in"
      <+> annotate FilePath (pretty dockerfile)

  viaManifest :: MonadIO m => FilePath -> MaybeT m (Doc Ann)
  viaManifest path = do
    let manifest = unpack dependency.depName </> path
    contents <- readFileExists manifest

    guard $ dependency.packageName `T.isInfixOf` contents

    pure
      $ "configured by manifest"
      <+> annotate FilePath (pretty manifest)

readFileExists :: MonadIO m => FilePath -> MaybeT m Text
readFileExists f = do
  guard =<< liftIO (doesFileExist f)
  liftIO $ T.readFile f

getRenovatePragmaLine :: Text -> Maybe Int
getRenovatePragmaLine =
  fmap fst
    . find (isRenovatePragma . snd)
    . zip [1 ..]
    . T.lines

isRenovatePragma :: Text -> Bool
isRenovatePragma = ("# renovate: " `T.isPrefixOf`)

manifestPaths :: [FilePath]
manifestPaths =
  [ "Gemfile"
  , "composer.json"
  , "package.json"
  , "requirements.txt"
  , "stack.yaml"
  ]
