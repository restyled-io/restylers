  $ source "$TESTDIR/helper.sh"
  If you don't see this, setup failed

stylish-haskell

  $ run_restyler stylish-haskell --inplace -- pragmas.hs
  diff --git i/pragmas.hs w/pragmas.hs
  index fd4c8c8..a07dc58 100644
  --- i/pragmas.hs
  +++ w/pragmas.hs
  @@ -1,2 +1,2 @@
   {-# LANGUAGE OverloadedStrings #-}
  -{-# LANGUAGE RecordWildCards #-}
  +{-# LANGUAGE RecordWildCards   #-}

MultiParam regression

  $ run_restyler stylish-haskell --inplace -- App.hs
  diff --git i/App.hs w/App.hs
  index 125bd49..2814620 100644
  --- i/App.hs
  +++ w/App.hs
  @@ -1,9 +1,9 @@
  -{-# LANGUAGE FlexibleContexts #-}
  -{-# LANGUAGE FlexibleInstances #-}
  -{-# LANGUAGE GADTs #-}
  +{-# LANGUAGE FlexibleContexts           #-}
  +{-# LANGUAGE FlexibleInstances          #-}
  +{-# LANGUAGE GADTs                      #-}
   {-# LANGUAGE GeneralizedNewtypeDeriving #-}
  -{-# LANGUAGE OverloadedStrings #-}
  -{-# LANGUAGE RecordWildCards #-}
  +{-# LANGUAGE OverloadedStrings          #-}
  +{-# LANGUAGE RecordWildCards            #-}
   
   module Restyler.App
       (
  @@ -18,37 +18,37 @@ module Restyler.App
       )
   where
   
  -import Restyler.Prelude
  -
  -import Conduit (runResourceT, sinkFile)
  -import qualified Data.Text as T
  -import qualified Data.Text.IO as T
  -import qualified Data.Vector as V
  -import GitHub.Endpoints.Issues.Comments
  -import GitHub.Endpoints.PullRequests
  -import GitHub.Endpoints.Repos.Statuses
  -import GitHub.Request
  -import Network.HTTP.Client.TLS
  -import Network.HTTP.Simple hiding (Request)
  -import Restyler.Capabilities.Docker
  -import Restyler.Capabilities.Git
  -import Restyler.Capabilities.GitHub
  -import Restyler.Capabilities.RemoteFile
  -import Restyler.Capabilities.System
  -import Restyler.Model.Config
  -import Restyler.Model.RemoteFile
  -import qualified System.Directory as Directory
  -import qualified System.Exit as Exit
  -import System.Process
  +import           Restyler.Prelude
  +
  +import           Conduit                          (runResourceT, sinkFile)
  +import qualified Data.Text                        as T
  +import qualified Data.Text.IO                     as T
  +import qualified Data.Vector                      as V
  +import           GitHub.Endpoints.Issues.Comments
  +import           GitHub.Endpoints.PullRequests
  +import           GitHub.Endpoints.Repos.Statuses
  +import           GitHub.Request
  +import           Network.HTTP.Client.TLS
  +import           Network.HTTP.Simple              hiding (Request)
  +import           Restyler.Capabilities.Docker
  +import           Restyler.Capabilities.Git
  +import           Restyler.Capabilities.GitHub
  +import           Restyler.Capabilities.RemoteFile
  +import           Restyler.Capabilities.System
  +import           Restyler.Model.Config
  +import           Restyler.Model.RemoteFile
  +import qualified System.Directory                 as Directory
  +import qualified System.Exit                      as Exit
  +import           System.Process
   
   -- | Application environment
   data App = App
  -    { appLogLevel :: LogLevel
  -    , appLogColor :: Bool
  -    , appAccessToken :: Text
  -    , appPullRequest :: PullRequest
  +    { appLogLevel            :: LogLevel
  +    , appLogColor            :: Bool
  +    , appAccessToken         :: Text
  +    , appPullRequest         :: PullRequest
       -- ^ The @'PullRequest'@ we are restyling
  -    , appConfig :: Config
  +    , appConfig              :: Config
       -- ^ Configuration loaded from @.restyled.yaml@
       , appRestyledPullRequest :: Maybe SimplePullRequest
       -- ^ Existing restyled @'PullRequest'@ if it exists
