module RenovateAudit.DependencyDashboardSpec
  ( spec
  ) where

import Restylers.Prelude

import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as T
import RenovateAudit.DependencyDashboard
import Test.Hspec

spec :: Spec
spec = do
  describe "parseDependencyDashboardIssue" $ do
    it "works" $ do
      let expected =
            DependencyDashboard
              { unwrap =
                  Map.fromList
                    [
                      ( "@trivago/prettier-plugin-sort-imports"
                      , KnownDependency
                          { constraint = "^6.0.0"
                          , filePath = "prettier/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "Blammo"
                      , KnownDependency
                          { constraint = "2.1.3.0"
                          , filePath = "_tools/stack.yaml"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "Cabal-syntax"
                      , KnownDependency
                          { constraint = "3.12.1.0"
                          , filePath = "cabal-fmt/stack.yaml"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "actions/checkout"
                      , KnownDependency
                          { constraint = "v6"
                          , filePath = ".github/workflows/ci.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "actions/create-github-app-token"
                      , KnownDependency
                          { constraint = "v2"
                          , filePath = ".github/workflows/release.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "actions/download-artifact"
                      , KnownDependency
                          { constraint = "v7"
                          , filePath = ".github/workflows/ci.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "actions/setup-node"
                      , KnownDependency
                          { constraint = "v6"
                          , filePath = ".github/workflows/ci.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "actions/upload-artifact"
                      , KnownDependency
                          { constraint = "v6"
                          , filePath = ".github/workflows/ci.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "alpine"
                      , KnownDependency
                          { constraint = "3.23.3"
                          , filePath = "jq/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "amazoncorretto"
                      , KnownDependency
                          { constraint = "25-alpine3.22-jdk"
                          , filePath = "google-java-format/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "apply-refact"
                      , KnownDependency
                          { constraint = "0.15.0.0"
                          , filePath = "hlint/stack.yaml"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "autopep8"
                      , KnownDependency
                          { constraint = "==2.3.2"
                          , filePath = "autopep8/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "aws-actions/amazon-ecr-login"
                      , KnownDependency
                          { constraint = "v2"
                          , filePath = ".github/workflows/ci.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "aws-actions/configure-aws-credentials"
                      , KnownDependency
                          { constraint = "v6"
                          , filePath = ".github/workflows/ci.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "black"
                      , KnownDependency
                          { constraint = "==26.1.0"
                          , filePath = "black/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "brittany"
                      , KnownDependency
                          { constraint = "0.14.02"
                          , filePath = "brittany/stack.yaml"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "cabal-fmt"
                      , KnownDependency
                          { constraint = "0.1.12"
                          , filePath = "cabal-fmt/stack.yaml"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "clazy"
                      , KnownDependency
                          { constraint = "v1.16"
                          , filePath = "clazy/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "click"
                      , KnownDependency
                          { constraint = "==8.3.1"
                          , filePath = "black/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "cmakelang"
                      , KnownDependency
                          { constraint = "==0.6.13"
                          , filePath = "cmake-format/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "csanchez/maven"
                      , KnownDependency
                          { constraint = "4.0-eclipse-temurin-17-maven-4"
                          , filePath = "jdt/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "cycjimmy/semantic-release-action"
                      , KnownDependency
                          { constraint = "v4"
                          , filePath = ".github/workflows/release.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "dart"
                      , KnownDependency
                          { constraint = "3.10.9"
                          , filePath = "dart-format/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "debian"
                      , KnownDependency
                          { constraint = "trixie"
                          , filePath = "astyle/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "dfmt"
                      , KnownDependency
                          { constraint = "v0.14.2"
                          , filePath = "dfmt/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "dlanguage/dmd"
                      , KnownDependency
                          { constraint = "2.080.0"
                          , filePath = "dfmt/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "elm"
                      , KnownDependency
                          { constraint = "0.19.1-6"
                          , filePath = "elm-format/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "elm-format"
                      , KnownDependency
                          { constraint = "0.8.8"
                          , filePath = "elm-format/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "fourmolu"
                      , KnownDependency
                          { constraint = "0.19.0.1"
                          , filePath = "fourmolu/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "freckle/stack-action"
                      , KnownDependency
                          { constraint = "v5"
                          , filePath = ".github/workflows/ci.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "friendsofphp/php-cs-fixer"
                      , KnownDependency
                          { constraint = "3.93.1"
                          , filePath = "php-cs-fixer/composer.json"
                          , manager = "composer"
                          }
                      )
                    ,
                      ( "golang"
                      , KnownDependency
                          { constraint = "1.25-alpine"
                          , filePath = "gofmt/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "google-java-format"
                      , KnownDependency
                          { constraint = "1.34.1"
                          , filePath = "google-java-format/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "headroom"
                      , KnownDependency
                          { constraint = "0.4.3.0"
                          , filePath = "headroom/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "hindent"
                      , KnownDependency
                          { constraint = "6.3.0"
                          , filePath = "hindent/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "hlint"
                      , KnownDependency
                          { constraint = "3.10"
                          , filePath = "hlint/stack.yaml"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "isort"
                      , KnownDependency
                          { constraint = "==7.0.0"
                          , filePath = "isort/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "jdt"
                      , KnownDependency
                          { constraint = "2.29.0"
                          , filePath = "jdt/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "jq"
                      , KnownDependency
                          { constraint = "1.6"
                          , filePath = "jq/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "monad-validate"
                      , KnownDependency
                          { constraint = "1.3.0.0"
                          , filePath = "_tools/stack.yaml"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "nixos/nix"
                      , KnownDependency
                          { constraint = "2.33.2"
                          , filePath = "dhall-format/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "node"
                      , KnownDependency
                          { constraint = "24"
                          , filePath = "elm-format/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "npm-groovy-lint"
                      , KnownDependency
                          { constraint = "16.2.0"
                          , filePath = "npm-groovy-lint/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "openjdk"
                      , KnownDependency
                          { constraint = "25-slim"
                          , filePath = "scalafmt/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "ormolu"
                      , KnownDependency
                          { constraint = "0.5.3.0"
                          , filePath = "ormolu/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "perl"
                      , KnownDependency
                          { constraint = "5"
                          , filePath = "perltidy/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "php"
                      , KnownDependency
                          { constraint = "8.1.13-cli-alpine3.15"
                          , filePath = "php-cs-fixer/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "prettier"
                      , KnownDependency
                          { constraint = "'~> 3.2.0'"
                          , filePath = "prettier-ruby/Gemfile"
                          , manager = "bundler"
                          }
                      )
                    ,
                      ( "prettier-plugin-svelte"
                      , KnownDependency
                          { constraint = "^3.3.2"
                          , filePath = "prettier/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "prettier-plugin-tailwindcss"
                      , KnownDependency
                          { constraint = "^0.7.0"
                          , filePath = "prettier/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "purty"
                      , KnownDependency
                          { constraint = "7.0.0"
                          , filePath = "purty/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "pycodestyle"
                      , KnownDependency
                          { constraint = "==2.14.0"
                          , filePath = "autopep8/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "pyment"
                      , KnownDependency
                          { constraint = "==0.3.3"
                          , filePath = "pyment/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "python"
                      , KnownDependency
                          { constraint = "3.14-alpine"
                          , filePath = "autopep8/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "pyyaml"
                      , KnownDependency
                          { constraint = ">=5.3"
                          , filePath = "cmake-format/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "reason-cli"
                      , KnownDependency
                          { constraint = "3.3.3-linux-1"
                          , filePath = "refmt/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "reorder-python-imports"
                      , KnownDependency
                          { constraint = "==3.16.0"
                          , filePath = "reorder-python-imports/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "restyled-io/actions"
                      , KnownDependency
                          { constraint = "v4"
                          , filePath = ".github/workflows/ci.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "restyled/stack-build-minimal"
                      , KnownDependency
                          { constraint = "24.04"
                          , filePath = "brittany/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "rickstaa/action-create-tag"
                      , KnownDependency
                          { constraint = "v1"
                          , filePath = ".github/workflows/promote.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "rubocop"
                      , KnownDependency
                          { constraint = "'~> 1.84.0'"
                          , filePath = "rubocop/Gemfile"
                          , manager = "bundler"
                          }
                      )
                    ,
                      ( "rubocop-performance"
                      , KnownDependency
                          { constraint = "'~> 1.26.0'"
                          , filePath = "rubocop/Gemfile"
                          , manager = "bundler"
                          }
                      )
                    ,
                      ( "rubocop-rails"
                      , KnownDependency
                          { constraint = "'~> 2.34.0'"
                          , filePath = "rubocop/Gemfile"
                          , manager = "bundler"
                          }
                      )
                    ,
                      ( "ruby"
                      , KnownDependency
                          { constraint = "4"
                          , filePath = "prettier-ruby/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "shellcheck"
                      , KnownDependency
                          { constraint = "0.11.0"
                          , filePath = "shellcheck/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "shellharden"
                      , KnownDependency
                          { constraint = "4.1.1"
                          , filePath = "shellharden/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "shfmt"
                      , KnownDependency
                          { constraint = "3.12.0"
                          , filePath = "shfmt/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "softprops/action-gh-release"
                      , KnownDependency
                          { constraint = "v2"
                          , filePath = ".github/workflows/promote.yml"
                          , manager = "github-actions"
                          }
                      )
                    ,
                      ( "sqlparse"
                      , KnownDependency
                          { constraint = "==0.5.5"
                          , filePath = "sqlformat/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "standard"
                      , KnownDependency
                          { constraint = "'~> 1.53.0'"
                          , filePath = "standardrb/Gemfile"
                          , manager = "bundler"
                          }
                      )
                    ,
                      ( "stylish-haskell"
                      , KnownDependency
                          { constraint = "0.14.3.0"
                          , filePath = "stylish-haskell/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "svelte"
                      , KnownDependency
                          { constraint = "^5.0.0"
                          , filePath = "prettier/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "tailwindcss"
                      , KnownDependency
                          { constraint = "^4.0.0"
                          , filePath = "prettier/package.json"
                          , manager = "npm"
                          }
                      )
                    ,
                      ( "tamasfe/taplo"
                      , KnownDependency
                          { constraint = "0.9.3-alpine"
                          , filePath = "taplo/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "toml"
                      , KnownDependency
                          { constraint = "==0.10.2"
                          , filePath = "yapf/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ,
                      ( "ubuntu"
                      , KnownDependency
                          { constraint = "24.04"
                          , filePath = "brittany/Dockerfile"
                          , manager = "dockerfile"
                          }
                      )
                    ,
                      ( "verible"
                      , KnownDependency
                          { constraint = "v0.0-4051-g9fdb4057"
                          , filePath = "verible/Dockerfile"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "whitespace"
                      , KnownDependency
                          { constraint = "v0.2.0.5"
                          , filePath = "whitespace/info.yaml"
                          , manager = "regex"
                          }
                      )
                    ,
                      ( "yapf"
                      , KnownDependency
                          { constraint = "==0.43.0"
                          , filePath = "yapf/requirements.txt"
                          , manager = "pip_requirements"
                          }
                      )
                    ]
              }

      exampleIssue <- T.readFile "test/files/dependency-dashboard-issue.md"

      case parseDependencyDashboardIssue exampleIssue of
        Left err -> expectationFailure err
        Right actual -> actual `shouldBe` expected
