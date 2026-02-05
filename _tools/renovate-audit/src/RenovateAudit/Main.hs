module RenovateAudit.Main
  ( main
  ) where

import Restylers.Prelude

import Blammo.Logging.Logger
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text qualified as T
import Lens.Micro (to)
import Prettyprinter
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text qualified as Text
import RenovateAudit.App
import RenovateAudit.Audit (auditDependency, prettyAuditResult)
import RenovateAudit.Dependencies (Dependency (..), loadDependencies)
import RenovateAudit.DependencyDashboard (fromDependencyDashboardIssue)
import RenovateAudit.DependencyDashboard qualified as DependencyDashboard
import RenovateAudit.GitHub qualified as GitHub
import RenovateAudit.Options
import RenovateAudit.Pretty (annToAnsi)
import RenovateAudit.PullRequests (toRenovatePullRequests)
import RenovateAudit.PullRequests qualified as PullRequests

main :: IO ()
main = do
  opts <- parseOptions

  runAppT opts $ do
    dependencies <- loadDependencies
    logInfo $ "Loaded dependencies" :# ["count" .= length dependencies]

    mIssue <- GitHub.getOpenIssueByTitle "Dependency Dashboard"

    dashboard <- case mIssue of
      Nothing -> logWarn "Dependency Dashboard not found" >> pure mempty
      Just issue -> do
        logDebug $ "Loaded Dependency Dashboard issue" :# ["number" .= issue.number]
        pure $ maybe mempty fromDependencyDashboardIssue mIssue

    logInfo
      $ "Parsed renovate-detected dependencies"
      :# ["count" .= DependencyDashboard.size dashboard]

    prs <- GitHub.foldMapClosedPullRequests $ foldMap toRenovatePullRequests

    logInfo
      $ "Fetched renovate dependency PRs"
      :# ["count" .= PullRequests.size prs]

    shouldColor <- asks $ view $ loggerL . to getLoggerShouldColor

    let
      render =
        if shouldColor
          then Ansi.renderStrict . reAnnotateS annToAnsi
          else Text.renderStrict

      -- Get width by longest dependency name
      w = maximum $ map (T.length . (.depName)) dependencies

    docs <- for dependencies $ \dependency -> do
      result <- auditDependency dashboard prs dependency
      pure $ prettyAuditResult w result

    pushLoggerLn
      $ render
      $ layoutPretty defaultLayoutOptions
      $ vsep
      $ map snd
      $ sortBy (comparing fst) docs
