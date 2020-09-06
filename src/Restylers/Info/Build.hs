{-# LANGUAGE NamedFieldPuns #-}

-- | Details about how to build a Restyler Docker image
--
-- TODO: We should load this from info.yaml along with everything else, and
-- default it to this current behavior.
--
module Restylers.Info.Build
    ( RestylerBuild(..)
    , restylerBuild
    , build
    )
where

import RIO

import Restylers.Image
import Restylers.Options (NoCache, addNoCache)
import RIO.FilePath (takeDirectory, (</>))
import RIO.Process
import RIO.Text (unpack)

data RestylerBuild = RestylerBuild
    { path :: FilePath
    , dockerfile :: FilePath
    , options :: [String]
    }
    deriving (Eq, Show)

restylerBuild :: FilePath -> RestylerBuild
restylerBuild yaml =
    let path = takeDirectory yaml
    in RestylerBuild { path, dockerfile = path </> "Dockerfile", options = [] }

build
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => NoCache
    -> RestylerBuild
    -> RestylerImage
    -> m RestylerImage
build noCache RestylerBuild {..} image = do
    logInfo $ "Building " <> display image
    image
        <$ proc
               "docker"
               (concat
                   [ ["build"]
                   , ["--tag", unImage image]
                   , ["--file", dockerfile]
                   , addNoCache noCache options
                   , [path]
                   ]
               )
               runProcess_

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
