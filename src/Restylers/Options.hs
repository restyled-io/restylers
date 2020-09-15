module Restylers.Options
    ( Command(..)
    , NoCache(..)
    , addNoCache
    , LintDockerfile(..)
    , whenLintDockerfile
    , RunTests(..)
    , whenRunTests
    , Push(..)
    , whenPush
    , Options(..)
    , HasOptions(..)
    , parseOptions
    )
where

import RIO

import Options.Applicative
import Restylers.Registry
import RIO.NonEmpty (some1)

newtype NoCache = NoCache Bool
    deriving newtype Show

addNoCache :: NoCache -> [String] -> [String]
addNoCache (NoCache b) = if b then ("--no-cache" :) else id

newtype LintDockerfile = LintDockerfile Bool
    deriving newtype Show

whenLintDockerfile :: Applicative m => LintDockerfile -> m () -> m ()
whenLintDockerfile (LintDockerfile b) = when b

newtype RunTests = RunTests Bool
    deriving newtype Show

whenRunTests :: Applicative m => RunTests -> m () -> m ()
whenRunTests (RunTests b) = when b

newtype Push = Push Bool
    deriving newtype Show

whenPush :: Applicative m => Push -> m () -> m ()
whenPush (Push b) = when b

data Command
    = Build NoCache LintDockerfile RunTests Push FilePath
    | Release FilePath (NonEmpty FilePath)
    deriving stock Show

data Options = Options
    { oRegistry :: Maybe Registry
    , oTag :: Text
    , oDebug :: Bool
    , oCommand :: Command
    }
    deriving stock Show

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
                <$> (NoCache <$> switch
                    (  long "no-cache"
                    <> help "Pass --no-cache to docker-build"
                    ))
                <*> (LintDockerfile <$> switch
                    (  long "lint"
                    <> help "Lint the build Dockerfile"
                    ))
                <*> (RunTests <$> switch
                    (  long "test"
                    <> help "Test the build image"
                    ))
                <*> (Push <$> switch
                    (  long "push"
                    <> help "Push the build image"
                    ))
                <*> yamlArgument
                )
            "Build an image for Restylers described in info.yaml files")
        <> command "release" (parse
            (Release
                <$> strOption
                    (  short 'w'
                    <> long "write"
                    <> help "File released Restylers to file"
                    <> metavar "PATH"
                    <> value "restylers.yaml"
                    )
                <*> some1 yamlArgument)
            "Released (push) versioned images")
        )

yamlArgument :: Parser FilePath
yamlArgument =
    argument str (help "Path to Restyler info.yaml" <> metavar "PATH")

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
