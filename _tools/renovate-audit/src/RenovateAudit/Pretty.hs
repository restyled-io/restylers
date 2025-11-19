module RenovateAudit.Pretty
  ( Ann (..)
  , annToAnsi
  , module Prettyprinter
  ) where

import Restylers.Prelude

import Prettyprinter
import Prettyprinter.Render.Terminal

data Ann
  = Header
  | Failure
  | Warning
  | Success
  | DepName
  | Manager
  | FilePath
  | LineNumber
  | Constraint
  | DetailCallout

annToAnsi :: Ann -> AnsiStyle
annToAnsi = \case
  Header -> bold
  Failure -> colorDull Red
  Warning -> colorDull Yellow
  Success -> colorDull Green
  DepName -> colorDull Magenta
  Manager -> colorDull Green
  FilePath -> underlined <> colorDull Cyan
  LineNumber -> colorDull Blue
  Constraint -> bold <> colorDull Blue
  DetailCallout -> colorDull Cyan
