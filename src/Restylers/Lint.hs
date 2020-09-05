

module Restylers.Lint
    ( lintRestylerImage
    )
where

import RIO

import Data.Aeson
import Data.Semigroup (getLast)
import Restylers.Build (mkBuildPath)
import qualified Restylers.Info as Info
import Restylers.Options
import Restylers.Restyler (loadRestylerInfo)
import RIO.Directory (getCurrentDirectory)
import RIO.FilePath ((</>))
import RIO.Process
import qualified RIO.Text as T

data LintError = LintError
    { code :: Text
    , message :: Text
    , level :: Text
    , file :: FilePath
    , line :: Natural
    , column :: Natural
    }
    deriving stock Generic
    deriving anyclass FromJSON

instance Display LintError where
    display LintError {..} =
        displayShow line
            <> ":"
            <> displayShow column
            <> " ["
            <> display level
            <> "] "
            <> display code
            <> ": "
            <> display message
            <> maybe "" (("\nSee " <>) . display) (wiki code)

wiki :: Text -> Maybe Text
wiki code
    | "DL" `T.isPrefixOf` code
    = Just $ "https://github.com/hadolint/hadolint/wiki/" <> code
    | "SC" `T.isPrefixOf` code
    = Just $ "https://github.com/hadolint/hadolint/wiki/" <> code
    | otherwise
    = Nothing

lintRestylerImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       )
    => FilePath
    -> m Bool
lintRestylerImage yaml = do
    (info, dockerfile) <- loadRestylerInfo yaml
        $ \info -> pure $ mkBuildPath info </> "Dockerfile"

    logDebug
        $ "Linting Dockerfile for "
        <> display (getLast $ Info.name info)
        <> " ("
        <> displayShow dockerfile
        <> ")"

    cwd <- getCurrentDirectory
    (ec, bs) <- proc
        "docker"
        (concat
            [ ["run", "--rm"]
            , ["--volume", cwd </> ".hadolint.yaml:/config.yaml:ro"]
            , ["--volume", cwd </> dockerfile <> ":/Dockerfile:ro"]
            , ["hadolint/hadolint", "hadolint"]
            , ["--config", "/config.yaml"]
            , ["--format", "json"]
            , ["/Dockerfile"]
            ]
        )
        readProcessStdout

    logDebug $ "hadolint exit code: " <> displayShow ec
    logDebug $ "hadolint stdout: " <> displayShow bs

    case eitherDecode bs of
        Left err -> throwString $ "Unable to read hadolint output: " <> err
        Right (errors :: [LintError]) -> do
            unless (null errors)
                $ logError
                $ "Lint errors found in "
                <> fromString dockerfile
            traverse_ (logError . display) errors

    pure $ ec /= ExitSuccess
