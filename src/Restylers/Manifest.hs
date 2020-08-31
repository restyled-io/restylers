module Restylers.Manifest
    ( RestylerManifest
    , HasRestylerManifest(..)
    , load
    , lookup
    , Restyler
    , name
    , image
    )
where

import RIO hiding (lookup)

import Data.Aeson
import qualified Data.Yaml as Yaml
import Restylers.Image
import Restylers.Name
import qualified RIO.HashMap as HashMap

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
    deriving anyclass FromJSON


load :: MonadIO m => FilePath -> m RestylerManifest
load path = do
    restylers <- Yaml.decodeFileThrow path
    pure $ RestylerManifest $ HashMap.fromList $ map (name &&& id) restylers

lookup :: RestylerName -> RestylerManifest -> Maybe Restyler
lookup name = HashMap.lookup name . unRestylerManifest
