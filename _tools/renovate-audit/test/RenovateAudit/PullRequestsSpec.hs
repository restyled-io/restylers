module RenovateAudit.PullRequestsSpec
  ( spec
  ) where

import Restylers.Prelude

import RenovateAudit.Dependencies (Dependency (..))
import RenovateAudit.GitHub (PullRequest (..))
import RenovateAudit.PullRequests (parsePullRequestDependency)
import Test.Hspec

spec :: Spec
spec = do
  describe "parsePullRequestDependency" $ do
    it "parses when dependency and package are the same" $ do
      let pr =
            PullRequest
              { number = 1
              , title = "feat(pg_format): update dependency pg_format to v5.9"
              , body = Nothing
              , closed_at = Nothing
              }

      parsePullRequestDependency pr
        `shouldBe` Right
          Dependency
            { depName = "pg_format"
            , packageName = "pg_format"
            }

    it "parses when dependency and package differ" $ do
      let pr =
            PullRequest
              { number = 1
              , title =
                  "feat(php-cs-fixer): update dependency friendsofphp/php-cs-fixer to v3.93.0"
              , body = Nothing
              , closed_at = Nothing
              }

      parsePullRequestDependency pr
        `shouldBe` Right
          Dependency
            { depName = "php-cs-fixer"
            , packageName = "friendsofphp/php-cs-fixer"
            }
