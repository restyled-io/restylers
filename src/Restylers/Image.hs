module Restylers.Image
    ( RestylerImage(..)
    )
where

import RIO

import Data.Aeson

newtype RestylerImage = RestylerImage
    { unRestylerImage :: Text
    }
    deriving newtype (FromJSON, ToJSON)
