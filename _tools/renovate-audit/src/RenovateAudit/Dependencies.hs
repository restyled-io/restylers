module RenovateAudit.Dependencies
  ( Dependency (..)
  , loadDependencies
  ) where

import Restylers.Prelude

import Data.List.Extra (nubOrdOn, sortOn)
import Restylers.Info.Resolved (RestylerInfo (..))
import Restylers.Info.Resolved qualified as Info
import Restylers.Name (RestylerName (..))
import Restylers.Override qualified as Override
import System.FilePath.Glob (glob)

data Dependency = Dependency
  { depName :: Text
  , packageName :: Text
  }
  deriving stock (Eq, Show)

loadDependencies :: MonadIO m => m [Dependency]
loadDependencies = do
  infos <- liftIO $ glob "*/info.yaml"
  sortOn (.depName)
    . nubOrdOn (.depName)
    . catMaybes
    <$> traverse loadDependency infos

loadDependency :: MonadIO m => FilePath -> m (Maybe Dependency)
loadDependency info = do
  eOverride <- Override.load info
  case eOverride of
    Left {} -> Just . restylerToDependency <$> Info.load info
    Right {} -> pure Nothing -- skip overrides

restylerToDependency :: RestylerInfo -> Dependency
restylerToDependency info
  | Just packageName <- lookup depName packageNames =
      Dependency {depName, packageName}
  | otherwise = Dependency {depName, packageName = depName}
 where
  depName = info.name.unwrap

packageNames :: [(Text, Text)]
packageNames =
  [ ("cmake-format", "cmakelang")
  , ("google-java-format", "com.google.googlejavaformat:google-java-format")
  , ("php-cs-fixer", "friendsofphp/php-cs-fixer")
  , ("prettier-ruby", "prettier")
  , ("sqlformat", "sqlparse")
  , ("standardrb", "standard")
  , ("taplo", "tamasfe/taplo")
  ]
