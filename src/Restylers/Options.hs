module Restylers.Options
    ( Command(..)
    , Options(..)
    , HasOptions(..)
    , parseOptions
    )
where

import RIO

import Options.Applicative
import Restylers.CommitSHA
import Restylers.Name
import Restylers.Registry
import RIO.NonEmpty (some1)

data Command
    = Release
    | Build (NonEmpty RestylerName) CommitSHA
    deriving Show

data Options = Options
    { oRegistry :: Registry
    , oManifest :: FilePath
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
    <$> (Registry <$> strOption
        (  short 'r'
        <> long "registry"
        <> help "Registry to prefix all Docker images"
        <> metavar "PREFIX"
        <> value "restyled"
        ))
    <*> strOption
        (  short 'm'
        <> long "manifest"
        <> help "File to read/write released Restylers manifest"
        <> metavar "PATH"
        <> value "restylers.yaml"
        )
    <*> switch
        (  short 'd'
        <> long "debug"
        <> help "Log more verbosity"
        )
    <*> subparser
        (  command "release" (parse (pure Release) "Release un-released Manifest images")
        <> command "build" (parse buildCommand "Build and test Restylers")
        )

-- brittany-disable-next-binding

buildCommand :: Parser Command
buildCommand = Build
    <$> some1 (RestylerName <$> strOption
        (  short 'n'
        <> long "name"
        <> help "Restyler name to build and test"
        <> metavar "NAME"
        ))
    <*> (CommitSHA <$> strOption
        (  short 'c'
        <> long "commit"
        <> help "Commit SHA to tag test builds"
        <> metavar "SHA"
        <> value "8d2a6e854a51ed2f0139675538ff002a214837f1" -- TODO
        ))

parse :: Parser a -> String -> ParserInfo a
parse p d = info (p <**> helper) $ fullDesc <> progDesc d
