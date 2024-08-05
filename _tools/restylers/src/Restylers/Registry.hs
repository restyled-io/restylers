module Restylers.Registry
  ( Registry (..)
  )
where

import Restylers.Prelude

newtype Registry = Registry
  { unwrap :: Text
  }
  deriving stock (Eq, Show)
