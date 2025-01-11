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

import Data.Text.IO qualified as T
import Data.Yaml qualified as Yaml
import Restylers.Docs.Render
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ref, path] -> do
      restylers <- Yaml.decodeFileThrow path
      T.putStrLn $ renderDocs (pack ref) restylers
    _ -> do
      hPutStrLn stderr "Usage: restyler-docs <ref> <manifest>"
      exitWith $ ExitFailure 64
