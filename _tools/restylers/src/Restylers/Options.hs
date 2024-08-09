-- |
--
-- Module      : Restylers.Options
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Options
  ( Options (..)
  , HasOptions (..)
  , parseOptions
  ) where

import Restylers.Prelude

import Data.List.NonEmpty (some1)
import Options.Applicative
import Restylers.Registry
import ShellWords qualified

data Options = Options
  { registry :: Maybe Registry
  , sha :: Text
  , debug :: Bool
  , build :: Bool
  , pull :: Bool
  , push :: Bool
  , write :: Maybe FilePath
  , checkForUpdate :: Bool
  -- ^ Ignored now. Will be parsed until we update restylers CI to not call it
  , input :: NonEmpty FilePath
  , hspecArgs :: Maybe [String]
  }
  deriving stock (Show)

class HasOptions env where
  optionsL :: Lens' env Options

parseOptions :: IO Options
parseOptions = execParser $ withInfo "Build, test, and push Restylers" options

-- brittany-disable-next-binding

options :: Parser Options
options =
  Options
    <$> optional
      ( Registry
          <$> strOption
            ( short 'r'
                <> long "registry"
                <> help "Registry to prefix all Docker images"
                <> metavar "PREFIX"
            )
      )
    <*> strOption
      ( short 's'
          <> long "sha"
          <> help "Commit SHA to use as tag for input images"
          <> metavar "SHA"
          <> value "dev"
      )
    <*> switch
      ( short 'd'
          <> long "debug"
          <> help "Log more verbosity"
      )
    <*> ( not
            <$> switch
              ( short 'B'
                  <> long "no-build"
                  <> help "Skip build before testing"
              )
        )
    <*> ( not
            <$> switch
              ( short 'P'
                  <> long "no-pull"
                  <> help "Pass --no-pull to restyle"
              )
        )
    <*> switch
      ( short 'p'
          <> long "push"
          <> help "Push version-tagged image"
      )
    <*> optional
      ( strOption
          ( short 'w'
              <> long "write"
              <> help "Output restyler definition to PATH"
              <> metavar "PATH"
          )
      )
    <*> switch (long "check-for-update" <> internal)
    <*> some1
      ( argument
          str
          ( help "Path to Restyler info.yaml"
              <> metavar "PATH"
          )
      )
    <*> optional
      ( option
          (eitherReader ShellWords.parse)
          ( long "hspec-arguments"
              <> help "Arguments for generated hspec test suite"
          )
      )

withInfo :: String -> Parser a -> ParserInfo a
withInfo d p = info (p <**> helper) $ fullDesc <> progDesc d
