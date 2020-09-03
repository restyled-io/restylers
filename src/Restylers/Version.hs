module Restylers.Version
    ( RestylerVersion(..)
    )
where

import RIO

import Data.Aeson

newtype RestylerVersion = RestylerVersion
    { unRestylerVersion :: Text
    }
    deriving newtype (Display, FromJSON)
