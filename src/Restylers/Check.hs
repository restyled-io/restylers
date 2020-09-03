module Restylers.Check
    ( RestylersCheckError
    , restylersCheckErrorRestyler
    , checkRestylersImage
    )
where

import RIO

import Restylers.Manifest (HasRestylerManifest)
import qualified Restylers.Manifest as Manifest
import Restylers.Options
import Restylers.Release
import Restylers.Restyler (Restyler)
import qualified Restylers.Restyler as Restyler
import RIO.Process

data RestylersCheckError
    = UnexpectedRestyler Restyler
    | RestylerChangedFrom Restyler Restyler

instance Display RestylersCheckError where
    display = \case
        UnexpectedRestyler restyler ->
            "Restyler not present in manifest: "
                <> "\n===\n"
                <> display restyler
                <> "\n==="
        RestylerChangedFrom original restyler ->
            "Restyler differs from what's present in the manifest:"
                <> "\n===\n"
                <> display restyler
                <> "\n=== differs from ===\n"
                <> display original
                <> "\n==="

restylersCheckErrorRestyler :: RestylersCheckError -> Restyler
restylersCheckErrorRestyler = \case
    UnexpectedRestyler restyler -> restyler
    RestylerChangedFrom _ restyler -> restyler

-- | Produce a 'Restyler' and check against the 'Manifest'
--
-- If there are differences, we return an error
--
checkRestylersImage
    :: ( MonadIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasOptions env
       , HasRestylerManifest env
       )
    => FilePath
    -> m (Either RestylersCheckError Restyler)
checkRestylersImage yaml = do
    restyler <- tagRestylerImage yaml
    mReleased <- Manifest.lookup $ Restyler.name restyler

    pure $ case mReleased of
        Nothing -> Left $ UnexpectedRestyler restyler
        Just released | released == restyler -> Right restyler
        Just released -> Left $ RestylerChangedFrom released restyler
