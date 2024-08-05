module Restylers.Options
  ( Options (..)
  , HasOptions (..)
  , parseOptions
  ) where

import RIO

import Options.Applicative
import RIO.NonEmpty (some1)
import Restylers.Registry
import qualified ShellWords

data Options = Options
  { oRegistry :: Maybe Registry
  , oSha :: Text
  , oDebug :: Bool
  , oBuild :: Bool
  , oPull :: Bool
  , oPush :: Bool
  , oWrite :: Maybe FilePath
  , oCheckForUpdate :: Bool
  -- ^ Ignored now. Will be parsed until we update restylers CI to not call it
  , oInput :: NonEmpty FilePath
  , oHspecArgs :: Maybe [String]
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
