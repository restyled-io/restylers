module Restylers.Manifest
    ( RestylerManifest
    , HasRestylerManifest(..)
    , load
    , lookup
    , writeUpdated
    )
where

import RIO hiding (lookup)

import qualified Data.Yaml as Yaml
import Restylers.Name
import Restylers.Restyler (Restyler)
import qualified Restylers.Restyler as Restyler
import qualified RIO.HashMap as HashMap
import RIO.List (sortOn)

newtype RestylerManifest = RestylerManifest
    { unRestylerManifest :: HashMap RestylerName Restyler
    }

class HasRestylerManifest env where
    manifestL :: Lens' env RestylerManifest

load :: MonadIO m => FilePath -> m RestylerManifest
load = fmap (RestylerManifest . loadRestylers) . Yaml.decodeFileThrow

loadRestylers :: [Restyler] -> HashMap RestylerName Restyler
loadRestylers = HashMap.fromList . map (Restyler.name &&& id)

lookup :: RestylerName -> RestylerManifest -> Maybe Restyler
lookup name = HashMap.lookup name . unRestylerManifest

writeUpdated
    :: (MonadIO m, MonadReader env m, HasRestylerManifest env)
    => FilePath
    -> [Restyler]
    -> m ()
writeUpdated path restylers = do
    local (over manifestL $ unionInfos restylers) $ do
        manifest <- view manifestL
        liftIO
            $ Yaml.encodeFile path
            $ sortOn Restyler.name
            $ HashMap.elems
            $ unRestylerManifest manifest

unionInfos :: [Restyler] -> RestylerManifest -> RestylerManifest
unionInfos restylers (RestylerManifest hm) =
    RestylerManifest $ HashMap.union (loadRestylers restylers) hm
