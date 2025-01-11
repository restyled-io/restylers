{-# LANGUAGE QuasiQuotes #-}

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

import Data.Aeson (Value, object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Yaml qualified as Yaml
import Restylers.Image (RestylerImage (..))
import Restylers.Info.Metadata (Metadata (..))
import Restylers.Info.Test (Test (..))
import Restylers.Manifest (Restyler (..))
import Restylers.Name (RestylerName (..))
import Text.Shakespeare.Text (st)

renderDocs :: Text -> [Restyler] -> Text
renderDocs ref restylers =
  T.unlines
    $ [header ref]
    <> restylersTable restylers
    <> ["---"]
    <> map restylerSection restylers

header :: Text -> Text
header ref =
  [st|
# Restylers

Built from `#{ref}`
|]

restylersTable :: [Restyler] -> [Text]
restylersTable restylers = headerRows <> map restylerTableRow restylers
 where
  headerRows :: [Text]
  headerRows =
    [ "| Restyler | Language(s) | Version | Runs automatically? |"
    , "| -------- | ----------- | ------- | ------------------- |"
    ]

restylerTableRow :: Restyler -> Text
restylerTableRow restyler =
  "| " <> T.intercalate " | " (restylerTableCells restyler) <> " |"

restylerTableCells :: Restyler -> [Text]
restylerTableCells restyler =
  [ [st|[#{restyler.name}](##{restyler.name})|]
  , T.intercalate ", " restyler.metadata.languages
  , [st|`#{restyler.image.tag}`|]
  , if restyler.enabled then "Yes" else "No"
  ]

restylerSection :: Restyler -> Text
restylerSection restyler =
  [st|
## #{restyler.name}

#{description}

#{restylerDocs restyler}

#{restylerConfig restyler}

#{restylerExamples restyler}

[See all available images](https://gallery.ecr.aws/restyled-io/restyler-#{restyler.name})
|]
 where
  description :: Text
  description =
    "Restyles "
      <> T.intercalate ", " (map (\x -> "_" <> x <> "_") $ restyler.metadata.languages)
      <> ", "
      <> ( if restyler.enabled
            then "runs automatically"
            else "must be explicitly enabled"
         )
      <> "."

restylerDocs :: Restyler -> Text
restylerDocs = details "Documentation" . T.unlines . map ("- " <>) . (.documentation)

restylerConfig :: Restyler -> Text
restylerConfig = details "Configuration" . codeBlock "yaml" . restylersYaml

restylersYaml :: Restyler -> Text
restylersYaml = decodeUtf8 . Yaml.encode . restylerValue

restylerValue :: Restyler -> Value
restylerValue restyler =
  object
    [ "restylers"
        .= [ object
              [ key
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
 where
  key = Key.fromText $ restyler.name.unwrap

restylerExamples :: Restyler -> Text
restylerExamples restyler =
  details "Examples"
    $ T.unlines
    $ map
      (restylerExample $ listToMaybe restyler.metadata.languages)
      restyler.metadata.tests

restylerExample :: Maybe Text -> Test -> Text
restylerExample mlang test =
  [st|
**Before**

#{codeBlock lang test.contents}

**After**

#{codeBlock lang test.restyled}
|]
 where
  lang :: Text
  lang = maybe "" toLang mlang

  toLang :: Text -> Text
  toLang = \case
    "*" -> ""
    "C#" -> "csharp"
    "F#" -> "fsharp"
    "POSIX sh" -> "sh"
    "System Verilog" -> "verilog"
    x -> T.toLower x

details :: Text -> Text -> Text
details summary content =
  [st|
<details>
<summary>#{summary}</summary>

#{content}

</details>
|]

codeBlock :: Text -> Text -> Text
codeBlock lang contents =
  [st|
```#{lang}
#{contents}
```
|]
