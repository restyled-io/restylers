module Restylers.CommitSHA
    ( CommitSHA(..)
    )
where

import RIO

newtype CommitSHA = CommitSHA
    { unCommitSHA :: Text
    }
    deriving stock Show
