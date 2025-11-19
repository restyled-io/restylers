module RenovateAudit.Audit
  ( AuditResult (..)
  , auditDependency
  , prettyAuditResult
  ) where

import Restylers.Prelude

import Data.List (find)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import RenovateAudit.Config
import RenovateAudit.Dependencies (Dependency (..))
import RenovateAudit.DependencyDashboard
  ( DependencyDashboard
  , KnownDependency (..)
  )
import RenovateAudit.DependencyDashboard qualified as DependencyDashboard
import RenovateAudit.GitHub (PullRequest (..))
import RenovateAudit.Pretty

data AuditResult = AuditResult
  { dependency :: Dependency
  , configured :: Maybe (Doc Ann)
  , managed :: Maybe (Doc Ann)
  , updated :: Maybe (Doc Ann)
  }
  deriving stock (Show)

auditDependency
  :: MonadUnliftIO m
  => DependencyDashboard
  -> [PullRequest]
  -> Dependency
  -> m AuditResult
auditDependency dashboard prs dependency = do
  configured <- findRenovatedConfigured dependency

  pure
    AuditResult
      { dependency
      , configured
      , managed = knownDoc <$> DependencyDashboard.lookup dashboard dependency
      , updated = updatedDoc <$> find (pullRequestUpdates dependency) prs
      }

pullRequestUpdates :: Dependency -> PullRequest -> Bool
pullRequestUpdates dependency pr = expectedTitlePrefix `T.isInfixOf` pr.title
 where
  expectedTitlePrefix =
    "feat("
      <> dependency.depName
      <> "): update dependency "
      <> dependency.packageName
      <> " to"

knownDoc :: KnownDependency -> Doc Ann
knownDoc known =
  "managed by"
    <+> annotate Manager (pretty known.manager)
    <+> "in"
    <+> annotate FilePath (pretty known.filePath)
    <+> annotate Constraint (pretty known.constraint)

updatedDoc :: PullRequest -> Doc Ann
updatedDoc pr =
  "updated by"
    <+> annotate DetailCallout ("#" <> pretty pr.number)
    <+> "on"
    <+> annotate
      DetailCallout
      (pretty $ maybe "unknown" (formatTime defaultTimeLocale "%Y-%m-%d") pr.closed_at)

-- | Returns pretty sort key in first tuple element
prettyAuditResult :: Int -> AuditResult -> (Int, Doc Ann)
prettyAuditResult w result =
  case (result.configured, result.managed, result.updated) of
    (Nothing, Nothing, Nothing) ->
      ( 3
      , annotate Failure "✗"
          <+> depName
          <+> failure "not configured"
      )
    (Just doc, Nothing, Nothing) ->
      ( 2
      , annotate Failure "✗"
          <+> depName
          <+> failure "not managed"
          <+> doc
      )
    (_, Just doc, Nothing) ->
      ( 1
      , annotate Failure "✗"
          <+> depName
          <+> failure "not updated"
          <+> doc
      )
    (_, _, Just doc) ->
      ( 0
      , annotate Success "✓"
          <+> depName
          <+> annotate Success (fill labelW "ok")
          <+> doc
      )
 where
  depName = annotate DepName $ fill w $ pretty result.dependency.depName
  failure = annotate Failure . fill labelW
  labelW = 15
