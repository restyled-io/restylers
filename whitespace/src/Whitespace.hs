{-# LANGUAGE OverloadedStrings #-}

module Whitespace
  ( FormatOptions (..),
    formatPaths,
    format,
  )
where

import Control.Exception (Exception, SomeException, handle, throwIO)
import Control.Monad (when)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Char (isSpace)
import Data.Foldable (traverse_)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hPrint, hPutStrLn, stderr)

data FormatOptions
  = FormatOptions
      { -- | Trim trailing whitespace from lines?
        foSpaces :: Bool,
        -- | Fix newlines at end of file?
        foNewlines :: Bool,
        -- | Halt on errors reading files?
        foStrict :: Bool,
        -- | Files to process
        foPaths :: [FilePath]
      }

formatPaths :: FormatOptions -> IO ()
formatPaths opts = traverse_ (formatPath opts) $ foPaths opts

data UnableToFormat
  = UnableToFormatCRLF
  deriving (Show)

instance Exception UnableToFormat

formatPath :: FormatOptions -> FilePath -> IO ()
formatPath opts path =
  handle (onException (foStrict opts) path) $ do
    content <- BS.readFile path
    if isCRLF content
      then throwIO UnableToFormatCRLF
      else BS.writeFile path $ format opts content

isCRLF :: ByteString -> Bool
isCRLF = ("\r\n" `C8.isInfixOf`)

format :: FormatOptions -> ByteString -> ByteString
format opts = onOpt foNewlines newlines . onOpt foSpaces spaces
  where
    onOpt attr f = bool id f $ attr opts

-- | Ensure a single trailing newline
newlines :: ByteString -> ByteString
newlines = withEnd $ C8.cons '\n' . C8.dropWhile (== '\n')

-- | Trim whitespace from the end of all lines
spaces :: ByteString -> ByteString
spaces = eachLine $ withEnd $ C8.dropWhile isSpace

eachLine :: (ByteString -> ByteString) -> ByteString -> ByteString
eachLine f = C8.unlines . map f . C8.lines

withEnd :: (ByteString -> ByteString) -> ByteString -> ByteString
withEnd f = C8.reverse . f . C8.reverse

onException :: Bool -> FilePath -> SomeException -> IO ()
onException strict path ex = do
  hPutStrLn stderr $ "Exception processing " <> path <> ":"
  hPrint stderr ex
  when strict $ do
    hPutStrLn stderr "Aborting, disable strict mode to ignore."
    exitWith $ ExitFailure 1
