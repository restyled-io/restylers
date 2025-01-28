{-# LANGUAGE TemplateHaskell #-}

-- |
--
-- Module      : Restylers.Docs.Render
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Docs.Render
  ( renderDocs
  ) where

import Restylers.Docs.Prelude

import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy qualified as TL
import Data.Yaml qualified as Yaml
import GHC.Generics (Generic)
import Restylers.Image
import Restylers.Info.Metadata (Metadata (..))
import Restylers.Manifest (Restyler (..))
import Restylers.Name (RestylerName (..))
import Text.Mustache.Compile.TH
import Text.Mustache.Render

newtype TPayload = TPayload
  { restylers :: [RPayload]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data MPayload = MPayload
  { languagesList :: Text
  , languagesListI :: Text
  , languageGuess :: Text
  , imageTag :: Text
  , enabledYesNo :: Text
  , enabledDescription :: Text
  , infoYaml :: Text
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data RPayload = RPayload
  { restyler :: Restyler
  , metadata :: MPayload
  }

toRPayload :: Restyler -> RPayload
toRPayload restyler = RPayload {restyler, metadata = toMPayload restyler}

toMPayload :: Restyler -> MPayload
toMPayload restyler =
  MPayload
    { languagesList =
        T.intercalate ", "
          $ restyler.metadata.languages
    , languagesListI =
        T.intercalate ", "
          . map (\x -> "_" <> x <> "_")
          $ restyler.metadata.languages
    , languageGuess = guessLanguage
    , imageTag = restyler.image.tag.unwrap
    , enabledYesNo =
        if restyler.enabled
          then "Yes"
          else "No"
    , enabledDescription =
        if restyler.enabled
          then "runs automatically"
          else "must be explicitly enabled"
    , infoYaml =
        decodeUtf8
          $ Yaml.encode
          $ object
            [ "restylers"
                .= [ object
                       [ infoYamlKey
                           .= object
                             [ "image" .= restyler.image
                             , "command" .= restyler.command
                             , "arguments" .= restyler.arguments
                             , "include" .= restyler.include
                             , "interpreters" .= restyler.interpreters
                             ]
                       ]
                   ]
            ]
    }
 where
  infoYamlKey = Key.fromText $ restyler.name.unwrap

  guessLanguage =
    maybe
      ""
      ( \case
          "*" -> ""
          "C#" -> "csharp"
          "F#" -> "fsharp"
          "POSIX sh" -> "sh"
          "System Verilog" -> "verilog"
          x -> T.toLower x
      )
      . listToMaybe
      $ restyler.metadata.languages

instance ToJSON RPayload where
  toJSON p = case (toJSON p.restyler, toJSON p.metadata) of
    (Object a, Object b) -> Object $ KeyMap.union a b
    (x, _) -> x -- "impossible"

renderDocs :: [Restyler] -> Text
renderDocs =
  TL.toStrict
    . renderMustache template
    . toJSON
    . TPayload
    . map toRPayload
 where
  template = $(compileMustacheDir "template.md" ".")
