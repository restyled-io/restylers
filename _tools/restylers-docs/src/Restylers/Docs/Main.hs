-- |
--
-- Module      : Restylers.Docs.Main
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Docs.Main
  ( main
  ) where

import Restylers.Docs.Prelude

import Control.Applicative (asum)
import Control.Monad ((<=<))
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8
import Data.Text.IO qualified as T
import Data.Yaml qualified as Yaml
import Network.HTTP.Simple
  ( getResponseBody
  , getResponseStatus
  , httpLbs
  , parseRequest
  )
import Network.HTTP.Types.Status (statusIsSuccessful)
import Restylers.Docs.Render
import Restylers.Manifest (Restyler)
import System.Directory (doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import System.Process.Typed (ExitCode (..), proc, readProcess)

main :: IO ()
main = do
  docs <- renderDocs <$> getGitRef <*> decodeManifest
  T.putStrLn docs

getGitRef :: IO Text
getGitRef =
  pack
    <$> runMaybesOrDie
      "GH_SHA or GITHUB_SHA not set, and git-rev-parse failed"
      [ lookupEnv "GH_SHA"
      , lookupEnv "GITHUB_SHA"
      , readGit ["rev-parse", "HEAD"]
      ]

decodeManifest :: IO [Restyler]
decodeManifest = Yaml.decodeThrow =<< readManifest

readManifest :: IO ByteString
readManifest =
  runMaybesOrDie
    "No path argument, no ./restylers.yaml, and fetching manifest failed"
    [ traverse BS.readFile . listToMaybe =<< getArgs
    , readLocalManifest "restylers.yaml"
    , fetchRemoteManifest "dev"
    ]

readGit :: [String] -> IO (Maybe String)
readGit args = do
  (ec, out, err) <- readProcess $ proc "git" args
  case ec of
    ExitSuccess -> pure $ Just $ BSL8.unpack $ BSL8.dropWhileEnd (== '\n') out
    ExitFailure i -> do
      hPutStrLn stderr
        $ unlines
          [ "WARNING: reading git-rev-parse failed (" <> show i <> ")"
          , BSL8.unpack err
          ]
      pure Nothing

readLocalManifest :: FilePath -> IO (Maybe ByteString)
readLocalManifest path = do
  exists <- doesFileExist path
  if exists
    then Just <$> BS.readFile path
    else pure Nothing

fetchRemoteManifest :: String -> IO (Maybe ByteString)
fetchRemoteManifest tag = do
  req <- parseRequest url
  resp <- httpLbs req

  let
    status = getResponseStatus resp
    body = getResponseBody resp

  if statusIsSuccessful status
    then pure $ Just $ BSL.toStrict $ getResponseBody resp
    else do
      hPutStrLn stderr
        $ unlines
          [ "WARNING: fetching remote manifest failed"
          , "URL:    " <> url
          , "Status: " <> show status
          , "Body:   " <> BSL8.unpack body
          ]
      pure Nothing
 where
  url :: String
  url =
    "https://github.com/restyled-io/restylers/releases/download/"
      <> tag
      <> "/restylers.yaml"

runMaybesOrDie :: String -> [IO (Maybe a)] -> IO a
runMaybesOrDie msg = maybe (die msg) pure <=< runMaybeT . asum . map MaybeT
