-- |
--
-- Module      : Restylers.Registry
-- Copyright   : (c) 2024 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Restylers.Registry
  ( Registry (..)
  )
where

import Restylers.Prelude

newtype Registry = Registry
  { unwrap :: Text
  }
  deriving stock (Eq, Show)
