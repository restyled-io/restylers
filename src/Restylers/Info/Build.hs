{-# LANGUAGE NamedFieldPuns #-}

-- | Details about how to build a Restyler Docker image
module Restylers.Info.Build
    ( RestylerBuild(..)
    , restylerBuild
    , build
    )
where

import RIO

import Data.Aeson
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
    deriving stock (Eq, Show, Generic)
    deriving anyclass FromJSON

restylerBuild :: FilePath -> RestylerBuild
restylerBuild yaml = RestylerBuild
    { path
    , dockerfile = path </> "Dockerfile"
    , options = []
    }
    where path = takeDirectory yaml

build
    :: (MonadIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => NoCache
    -> RestylerBuild
    -> RestylerImage
    -> m RestylerImage
build noCache RestylerBuild {..} image = do
    logInfo $ "Building " <> display image

    let
        args = concat
            [ ["build"]
            , ["--tag", unImage image]
            , ["--file", dockerfile]
            , addNoCache noCache options
            , [path]
            ]

    image <$ proc "docker" args runProcess_

unImage :: RestylerImage -> String
unImage = unpack . unRestylerImage
