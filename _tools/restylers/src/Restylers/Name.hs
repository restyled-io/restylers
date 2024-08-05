module Restylers.Name
  ( RestylerName (..)
  )
where

import Restylers.Prelude

import Data.Aeson

newtype RestylerName = RestylerName
  { unwrap :: Text
  }
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)
