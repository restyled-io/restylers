module Restylers.Directory
  ( getCurrentDirectory
  , getCurrentHostDirectory
  , withCurrentDirectory
  ) where

import RIO

import RIO.Directory
import System.Environment (lookupEnv)

-- | N.B. this will be wrong within 'withCurrentDirectory'
getCurrentHostDirectory :: MonadIO m => m FilePath
getCurrentHostDirectory =
  maybe getCurrentDirectory pure =<< liftIO (lookupEnv "REALPWD")
