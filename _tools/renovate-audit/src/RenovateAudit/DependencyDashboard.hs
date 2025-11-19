module RenovateAudit.DependencyDashboard
  ( DependencyDashboard (..)
  , KnownDependency (..)
  , fromDependencyDashboardIssue
  , parseDependencyDashboardIssue
  , size
  , lookup
  ) where

import Restylers.Prelude hiding (lookup)

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.Foldable (fold)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Void (Void)
import RenovateAudit.Dependencies (Dependency (..))
import RenovateAudit.GitHub (PullRequest (..))
import Text.Megaparsec
import Text.Megaparsec.Char

newtype DependencyDashboard = DependencyDashboard
  { unwrap :: Map Text KnownDependency
  }
  deriving newtype (Semigroup, Monoid)
  deriving stock (Eq, Show)

data KnownDependency = KnownDependency
  { constraint :: Text
  , filePath :: FilePath
  , manager :: Text
  }
  deriving stock (Eq, Show)

size :: DependencyDashboard -> Int
size d = Map.size d.unwrap

lookup :: DependencyDashboard -> Dependency -> Maybe KnownDependency
lookup d dependency =
  Map.lookup dependency.packageName d.unwrap
    <|> Map.lookup dependency.depName d.unwrap

fromDependencyDashboardIssue :: PullRequest -> DependencyDashboard
fromDependencyDashboardIssue issue =
  case parseDependencyDashboardIssue issue.body of
    Left {} -> mempty
    Right dashboard -> dashboard

parseDependencyDashboardIssue :: Text -> Either String DependencyDashboard
parseDependencyDashboardIssue = first errorBundlePretty . parse issueP "<issue>"

type Parser = Parsec Void Text

issueP :: Parser DependencyDashboard
issueP = do
  void
    $ skipManyTill anySingle
    $ string "# Detected Dependencies"
    >> newline
    >> newline

  mdetails <- manyTill managerP $ string "---" >> newline
  toDependencyDashboard (fold mdetails) <$ skipManyTill anySingle eof

toDependencyDashboard :: ManagerDetails -> DependencyDashboard
toDependencyDashboard md = fold $ do
  (manager, files) <- Map.toList md.unwrap
  (file, packages) <- Map.toList files.unwrap
  PackageDetails {name, constraint} <- packages
  pure
    $ DependencyDashboard
    $ Map.singleton
      name
      KnownDependency
        { constraint = constraint
        , filePath = file
        , manager = manager
        }

newtype ManagerDetails = ManagerDetails
  { unwrap :: Map Text FileDetails
  }
  deriving newtype (Semigroup, Monoid)

managerP :: Parser ManagerDetails
managerP = do
  void $ string "<details><summary>"
  name <- someTill anySingle (char ' ' *> countP)
  void
    $ string "</summary>"
    >> newline
    >> string "<blockquote>"
    >> newline
    >> newline

  fdetails <-
    manyTill fileP
      $ string "</blockquote>"
      >> newline
      >> string "</details>"
      >> newline
      >> newline

  pure $ ManagerDetails $ Map.singleton (pack name) $ fold fdetails

newtype FileDetails = FileDetails
  { unwrap :: Map FilePath [PackageDetails]
  }
  deriving newtype (Semigroup, Monoid)

fileP :: Parser FileDetails
fileP = do
  void $ string "<details><summary>"
  name <-
    someTill
      anySingle
      (optional (char ' ' *> countP) >> string "</summary>" >> newline >> newline)
  pdetails <- sepEndBy packageP newline
  void $ newline >> string "</details>" >> newline >> newline
  pure $ FileDetails $ Map.singleton name pdetails

data PackageDetails = PackageDetails
  { name :: Text
  , constraint :: Text
  , _updates :: Maybe Text
  }

packageP :: Parser PackageDetails
packageP = do
  void $ string " - `"
  PackageDetails
    <$> (pack <$> manyTill anySingle (char ' '))
    <*> (pack <$> manyTill anySingle (char '`'))
    <*> optional updatesP

updatesP :: Parser Text
updatesP =
  string " → [Updates: `"
    *> (pack <$> manyTill anySingle (char '`'))
    <* char ']'

countP :: Parser Int
countP = char '(' *> digitsP <* char ')'

digitsP :: Parser Int
digitsP = read <$> some (satisfy isDigit)
