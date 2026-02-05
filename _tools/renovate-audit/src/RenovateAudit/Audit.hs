module RenovateAudit.Audit
  ( AuditResult (..)
  , auditDependency
  , prettyAuditResult
  ) where

import Restylers.Prelude

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Time (defaultTimeLocale, formatTime)
import RenovateAudit.Dependencies (Dependency (..))
import RenovateAudit.DependencyDashboard
  ( DependencyDashboard
  , KnownDependency (..)
  )
import RenovateAudit.DependencyDashboard qualified as DependencyDashboard
import RenovateAudit.GitHub (PullRequest (..))
import RenovateAudit.Pretty
import RenovateAudit.PullRequests (PullRequests)
import RenovateAudit.PullRequests qualified as PullRequests

data AuditResult = AuditResult
  { dependency :: Dependency
  , managed :: Maybe (Doc Ann)
  , updated :: Maybe (Doc Ann)
  }
  deriving stock (Show)

auditDependency
  :: MonadUnliftIO m
  => DependencyDashboard
  -> PullRequests
  -> Dependency
  -> m AuditResult
auditDependency dashboard prs dependency = do
  pure
    AuditResult
      { dependency
      , managed = knownDoc <$> DependencyDashboard.lookup dependency dashboard
      , updated = updatedDoc <$> PullRequests.lookup dependency prs
      }

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
  case (result.managed, result.updated) of
    (Nothing, Nothing) ->
      ( 2
      , annotate Failure "✗"
          <+> depName
          <+> annotate Failure (fill labelW "not managed")
      )
    (Just doc, Nothing) ->
      ( 1
      , annotate Warning "_"
          <+> depName
          <+> annotate Warning (fill labelW "not updated")
          <+> doc
      )
    (_, Just doc) ->
      ( 0
      , annotate Success "✓"
          <+> depName
          <+> annotate Success (fill labelW "ok")
          <+> doc
      )
 where
  depName = annotate DepName $ fill w $ pretty result.dependency.depName
  labelW = 11
