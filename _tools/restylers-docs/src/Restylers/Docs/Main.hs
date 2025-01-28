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
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = T.putStrLn . renderDocs =<< decodeManifest

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
