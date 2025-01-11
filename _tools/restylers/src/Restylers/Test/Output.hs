-- |
--
-- Module      : Restylers.Test.Output
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Test.Output
  ( frameOutput
  ) where

import Restylers.Prelude

import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 qualified as BSL8

{-

╭─┤ stdout ├───────
│ some output here
│ more output here
╰─

╭─┤ stderr ├───────
│ some output here
│ more output here
╰─

-}
frameOutput :: BSL.ByteString -> BSL.ByteString -> String
frameOutput stdout stderr =
  unlines
    $ map ("  " <>)
    $ frameOutputLines "stdout" (lines $ BSL8.unpack stdout)
    <> frameOutputLines "stderr" (lines $ BSL8.unpack stderr)

frameOutputLines :: String -> [String] -> [String]
frameOutputLines name = (headerLine :) . (<> [footerLine]) . map outLine
 where
  headerLine :: String
  outLine :: String -> String
  footerLine :: String

  -- Grouped below types so we can see it
  headerLine = "╭─┤ " <> name <> "├──────"
  outLine st = "│ " <> st
  footerLine = "╰─"
