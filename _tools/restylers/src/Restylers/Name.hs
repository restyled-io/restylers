-- |
--
-- Module      : Restylers.Name
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Name
  ( RestylerName (..)
  )
where

import Restylers.Prelude

import Data.Aeson

newtype RestylerName = RestylerName
  { unwrap :: Text
  }
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToText)
