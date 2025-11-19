module RenovateAudit.App
  ( App
  , AppT
  , runAppT
  ) where

import Restylers.Prelude

import Blammo.Logging.Logger
import Blammo.Logging.Simple
import RenovateAudit.GitHub (GitHubToken, HasGitHubToken (..), envGitHubToken)
import RenovateAudit.Options
import Restylers.LogFormat
import UnliftIO.Directory (withCurrentDirectory)

data App = App
  { options :: Options
  , logger :: Logger
  , githubToken :: GitHubToken
  }

instance HasLogger App where
  loggerL = lens (.logger) $ \x y -> x {logger = y}

instance HasGitHubToken App where
  getGithubToken = (.githubToken)

newtype AppT m a = AppT
  { unwrap :: ReaderT App m a
  }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader App
    , MonadUnliftIO
    )
  deriving (MonadLogger, MonadLoggerIO) via (WithLogger App m)

runAppT :: MonadUnliftIO m => Options -> AppT m a -> m a
runAppT options f = do
  with $ withLoggerEnv $ \logger -> do
    githubToken <- envGitHubToken
    runReaderT f.unwrap
      $ App
        { options
        , logger = setLoggerReformat reformatLoggedMessage logger
        , githubToken
        }
 where
  with = maybe id withCurrentDirectory options.cwd
