module RenovateAudit.PullRequests
  ( PullRequests
  , size
  , lookup
  , toRenovatePullRequests

    -- * For testing
  , parsePullRequestDependency
  ) where

import Restylers.Prelude hiding (lookup)

import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Void (Void)
import RenovateAudit.Dependencies (Dependency (..))
import RenovateAudit.GitHub (PullRequest (..))
import Text.Megaparsec
import Text.Megaparsec.Char

newtype PullRequests = PullRequests
  { unwrap :: Map Dependency PullRequest
  }
  deriving newtype (Semigroup, Monoid)

size :: PullRequests -> Int
size prs = Map.size prs.unwrap

lookup :: Dependency -> PullRequests -> Maybe PullRequest
lookup d prs = Map.lookup d prs.unwrap

toRenovatePullRequests :: PullRequest -> PullRequests
toRenovatePullRequests pr =
  case parsePullRequestDependency pr of
    Left {} -> mempty
    Right dep -> PullRequests $ Map.singleton dep pr

parsePullRequestDependency :: PullRequest -> Either String Dependency
parsePullRequestDependency pr =
  first errorBundlePretty $ parse titleP "<title>" pr.title

type Parser = Parsec Void Text

titleP :: Parser Dependency
titleP = do
  void $ string "feat("
  depName <- pack <$> manyTill anySingle (string "):")
  void $ hspace1 >> string "update dependency" >> hspace1
  packageName <- pack <$> manyTill anySingle (string " to ")
  Dependency {depName, packageName} <$ manyTill anySingle eof
