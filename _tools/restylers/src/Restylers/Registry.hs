module Restylers.Registry
  ( Registry (..)
  )
where

import Restylers.Prelude

newtype Registry = Registry
  { unRegistry :: Text
  }
  deriving stock (Eq, Show)
