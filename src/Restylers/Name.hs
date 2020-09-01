module Restylers.Name
    ( RestylerName(..)
    )
where

import RIO

import Data.Aeson

newtype RestylerName = RestylerName
    { unRestylerName :: Text
    }
    deriving newtype (Eq, Hashable, Ord, Show, Display, FromJSON, ToJSON)
