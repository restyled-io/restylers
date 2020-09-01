module Restylers.Registry
    ( Registry(..)
    )
where

import RIO

newtype Registry = Registry
    { unRegistry :: Text
    }
    deriving stock Show
