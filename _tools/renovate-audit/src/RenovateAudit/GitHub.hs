module RenovateAudit.GitHub
  ( GitHubToken
  , envGitHubToken
  , HasGitHubToken (..)
  , PullRequest (..)
  , foldMapClosedPullRequests
  , getOpenIssueByTitle
  ) where

import Restylers.Prelude

import Control.Applicative ((<|>))
import Control.Monad.Catch (Handler (..))
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Client (HttpExceptionContent (..))
import Network.HTTP.Simple
  ( HttpException (..)
  , Response
  , addRequestHeader
  , getResponseBody
  , getResponseHeader
  , getResponseStatus
  , httpJSON
  , parseRequestThrow
  )
import Network.HTTP.Types.Header
  ( HeaderName
  , hAuthorization
  , hRetryAfter
  , hUserAgent
  )
import Network.HTTP.Types.Status (status403, status429, statusCode)
import System.Environment (getEnv)
import Text.Read (readMaybe)
import UnliftIO.Retry

newtype GitHubToken = GitHubToken
  { unwrap :: ByteString
  }

envGitHubToken :: MonadIO m => m GitHubToken
envGitHubToken = liftIO $ GitHubToken . BS8.pack <$> getEnv "GITHUB_TOKEN"

class HasGitHubToken env where
  getGithubToken :: env -> GitHubToken

instance HasGitHubToken GitHubToken where
  getGithubToken = id

data PullRequest = PullRequest
  { number :: Int
  , title :: Text
  , body :: Maybe Text
  , closed_at :: Maybe UTCTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data SearchResponse = SearchResponse
  { total_count :: Int
  , items :: [PullRequest]
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

foldMapClosedPullRequests
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasGitHubToken env
     , Monoid w
     )
  => ([PullRequest] -> w)
  -> m w
foldMapClosedPullRequests f = go mempty 1
 where
  go acc p = do
    prs <-
      get @[PullRequest]
        $ "/repos/restyled-io/restylers/pulls"
        <> "?state=closed"
        <> "&sort=updated"
        <> "&direction=desc"
        <> "&per_page=100"
        <> "&page="
        <> show @Int p

    -- NB. Because we sort updated-descending, and we use (acc <>) below, the
    -- fact that Map's (<>) is the left-biased union means that the newest PR by
    -- dependency is what we get.

    if null prs
      then pure acc
      else go (acc <> f prs) (p + 1)

newtype Issue = Issue
  { title :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

getOpenIssueByTitle
  :: ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasGitHubToken env
     )
  => Text
  -> m (Maybe PullRequest)
getOpenIssueByTitle title = do
  resp <- get @SearchResponse $ "/search/issues" <> "?q=" <> unpack query
  pure $ listToMaybe resp.items
 where
  query :: Text
  query =
    T.intercalate
      "+"
      [ "repo:restyled-io%2Frestylers"
      , "is:issue"
      , "state:open"
      , "\"" <> title <> "\"+in:title"
      ]

get
  :: forall a m env
   . ( MonadUnliftIO m
     , MonadLogger m
     , MonadReader env m
     , HasGitHubToken env
     , FromJSON a
     )
  => String
  -> m a
get path = do
  token <- asks getGithubToken
  logDebugNS "github" $ "GET" :# ["path" .= path]
  req <- liftIO $ parseRequestThrow $ "https://api.github.com" <> path
  resp <-
    handleRateLimit
      . httpJSON
      . addRequestHeader hAuthorization ("token " <> token.unwrap)
      . addRequestHeader hUserAgent "restyled-io/renovate-audit"
      $ req
  pure $ getResponseBody resp

handleRateLimit
  :: (MonadUnliftIO m, MonadLogger m)
  => m (Response body)
  -> m (Response body)
handleRateLimit = recoveringDynamic policy [retryHandler] . const
 where
  policy = exponentialBackoff 50000 <> limitRetries 5

retryHandler
  :: (MonadIO m, MonadLogger m) => RetryStatus -> Handler m RetryAction
retryHandler rs = Handler $ \e -> do
  now <- liftIO getCurrentTime
  case e of
    HttpExceptionRequest _req (StatusCodeException resp _body)
      | Just a <- getRetryAfter now resp -> do
          logDebugNS "github"
            $ "Rate-limit"
            :# [ "status" .= statusCode (getResponseStatus resp)
               , "headers"
                   .= map
                     (\h -> (show h, show <$> lookupResponseHeader h resp))
                     [ "retry-after"
                     , "x-ratelimit-limit"
                     , "x-ratelimit-remaining"
                     , "x-ratelimit-reset"
                     ]
               , "retry" .= rs.rsIterNumber
               , "retryAfter" .= (show a.inSeconds <> "s")
               ]

          pure $ ConsultPolicyOverrideDelay a.inMicroSeconds
    _ -> pure DontRetry

data RetryAfter = RetryAfter
  { atTime :: UTCTime
  , inSeconds :: Int
  , inMicroSeconds :: Int
  }

retryAfterAtTime :: UTCTime -> UTCTime -> RetryAfter
retryAfterAtTime now atTime =
  RetryAfter
    { atTime
    , inSeconds
    , inMicroSeconds = inSeconds * 1000000
    }
 where
  inSeconds = round $ diffUTCTime atTime now

retryAfterInSeconds :: UTCTime -> Int -> RetryAfter
retryAfterInSeconds now inSeconds =
  RetryAfter
    { atTime = addUTCTime (fromIntegral inSeconds) now
    , inSeconds
    , inMicroSeconds = inSeconds * 1000000
    }

getRetryAfter :: UTCTime -> Response body -> Maybe RetryAfter
getRetryAfter now resp = do
  guard $ status `elem` [status429, status403]

  viaRetryAfterSeconds
    <|> viaRetryAfterDate
    <|> viaRateLimitRemaining
 where
  status = getResponseStatus resp

  viaRetryAfterSeconds :: Maybe RetryAfter
  viaRetryAfterSeconds =
    retryAfterInSeconds now . buffer <$> readResponseHeader hRetryAfter resp

  -- TODO
  viaRetryAfterDate :: Maybe RetryAfter
  viaRetryAfterDate = Nothing

  viaRateLimitRemaining :: Maybe RetryAfter
  viaRateLimitRemaining = do
    remaining <- readResponseHeader @Int "x-ratelimit-remaining" resp
    guard $ remaining == 0
    retryAfterAtTime now
      . posixSecondsToUTCTime
      . fromIntegral
      . buffer
      <$> readResponseHeader @Int "x-ratelimit-reset" resp

  buffer :: Int -> Int
  buffer = (+ 3) -- wait a bit extra avoid clock drift

lookupResponseHeader
  :: HeaderName
  -> Response body
  -> Maybe ByteString
lookupResponseHeader h = listToMaybe . getResponseHeader h

parseResponseHeader
  :: (ByteString -> Maybe a)
  -> HeaderName
  -> Response body
  -> Maybe a
parseResponseHeader p h = p <=< lookupResponseHeader h

readResponseHeader
  :: forall a body
   . Read a
  => HeaderName
  -> Response body
  -> Maybe a
readResponseHeader = parseResponseHeader $ readMaybe . BS8.unpack
