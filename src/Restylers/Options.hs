module Restylers.Options
    ( Command(..)
    , Options(..)
    , HasOptions(..)
    , parseOptions
    )
where

import RIO

import Options.Applicative
import Restylers.Registry
import RIO.NonEmpty (some1)

data Command
    = Build Bool Bool (NonEmpty FilePath)
    | Test (NonEmpty FilePath)
    | Lint (NonEmpty FilePath)
    | Release FilePath (NonEmpty FilePath)
    deriving Show

data Options = Options
    { oRegistry :: Maybe Registry
    , oTag :: Text
    , oDebug :: Bool
    , oCommand :: Command
    }
    deriving Show

class HasOptions env where
    optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = execParser $ parse options "Process Restylers"

-- brittany-disable-next-binding

options :: Parser Options
options = Options
    <$> optional (Registry <$> strOption
        (  short 'r'
        <> long "registry"
        <> help "Registry to prefix all Docker images"
        <> metavar "PREFIX"
        ))
    <*> strOption
        (  short 't'
        <> long "tag"
        <> help "Tag to use for development images"
        <> metavar "TAG"
        <> value "dev"
        )
    <*> switch
        (  short 'd'
        <> long "debug"
        <> help "Log more verbosity"
        )
    <*> subparser
        (  command "build" (parse
            (Build
                <$> switch
                    (  long "no-cache"
                    <> help "Pass --no-cache to docker-build"
                    )
                <*> switch
                    (  long "test"
                    <> help "Test the build image "
                    )
                <*> yamlsArgument
                )
            "Build an image for Restylers described in info.yaml files")
        <> command "test" (parse
            (Test <$> yamlsArgument)
            "Run tests for Restylers described in info.yaml files")
        <> command "lint" (parse
            (Lint <$> yamlsArgument)
            "Lint Restylers' Dockerfiles with hadolint")
        <> command "release" (parse
            (Release
                <$> strOption
                    (  short 'w'
                    <> long "write"
                    <> help "File released Restylers to file"
                    <> metavar "PATH"
                    <> value "restylers.yaml"
                    )
                <*> yamlsArgument)
            "Released (push) versioned images")
        )

yamlsArgument :: Parser (NonEmpty FilePath)
yamlsArgument =
    some1 (argument str (help "Path to Restyler info.yaml" <> metavar "PATH"))

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
