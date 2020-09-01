module Restylers.Manifest
    ( RestylerManifest
    , HasRestylerManifest(..)
    , load
    , lookup
    , writeUpdated
    , Restyler(..)
    )
where

import RIO hiding (lookup)

import Data.Aeson
import qualified Data.Yaml as Yaml
import Restylers.Image
import Restylers.Info (RestylerInfo)
import qualified Restylers.Info as Info
import Restylers.Name
import Restylers.Options
import Restylers.Registry
import qualified RIO.HashMap as HashMap
import RIO.List (sortOn)

newtype RestylerManifest = RestylerManifest
    { unRestylerManifest :: HashMap RestylerName Restyler
    }

class HasRestylerManifest env where
    manifestL :: Lens' env RestylerManifest

data Restyler = Restyler
    { name :: RestylerName
    , image :: RestylerImage
    }
    deriving stock Generic
    deriving anyclass (FromJSON, ToJSON)

load :: MonadIO m => FilePath -> m RestylerManifest
load = fmap (RestylerManifest . loadRestylers) . Yaml.decodeFileThrow

loadRestylers :: [Restyler] -> HashMap RestylerName Restyler
loadRestylers = HashMap.fromList . map (name &&& id)

lookup :: RestylerName -> RestylerManifest -> Maybe Restyler
lookup name = HashMap.lookup name . unRestylerManifest

writeUpdated
    :: (MonadIO m, MonadReader env m, HasOptions env, HasRestylerManifest env)
    => FilePath
    -> [RestylerInfo]
    -> m ()
writeUpdated path infos = do
    registry <- oRegistry <$> view optionsL
    local (over manifestL $ unionInfos registry infos) $ do
        manifest <- view manifestL
        liftIO
            $ Yaml.encodeFile path
            $ sortOn name
            $ HashMap.elems
            $ unRestylerManifest manifest

unionInfos :: Registry -> [RestylerInfo] -> RestylerManifest -> RestylerManifest
unionInfos registry infos (RestylerManifest hm) =
    RestylerManifest
        $ HashMap.union (loadRestylers $ map (fromInfo registry) infos) hm

fromInfo :: Registry -> RestylerInfo -> Restyler
fromInfo registry info =
    Restyler { name = Info.name info, image = Info.image info registry }
