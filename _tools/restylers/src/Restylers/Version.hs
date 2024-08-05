module Restylers.Version
  ( RestylerVersion (..)
  , toDataVersion
  , toSeriesMajor
  , toSeriesMinor
  ) where

import RIO

import Control.Error.Util (hush)
import Data.Aeson
import qualified Data.SemVer as SemVer
import Data.Version
import qualified RIO.NonEmpty as NE
import RIO.Text (pack, unpack)
import qualified RIO.Text as T
import Text.ParserCombinators.ReadP (readP_to_S)

newtype RestylerVersion = RestylerVersion
  { unRestylerVersion :: Text
  }
  deriving newtype (Eq, Show, Display, FromJSON, ToJSON)

toDataVersion :: RestylerVersion -> Maybe Version
toDataVersion =
  fmap (fst . NE.last)
    . NE.nonEmpty
    . readP_to_S parseVersion
    . dropV
    . unpack
    . unRestylerVersion
 where
  dropV = \case
    ('v' : rest) -> rest
    rest -> rest

-- | For a version like @vX.Y.Z@ returns @vX@
--
-- For this to work, the tag MUST be prefixed by @v@ and MUST be a valid
-- semantic version.
toSeriesMajor :: RestylerVersion -> Maybe Text
toSeriesMajor v = do
  sv <- toSemVerVersion v
  major <- sv ^? SemVer.major
  pure $ fromSemVerParts [major]

-- | For a version like @vX.Y.Z@ returns @vX.Y@
--
-- For this to work, the tag MUST be prefixed by @v@ and MUST be a valid
-- semantic version.
toSeriesMinor :: RestylerVersion -> Maybe Text
toSeriesMinor v = do
  sv <- toSemVerVersion v
  major <- sv ^? SemVer.major
  minor <- sv ^? SemVer.minor
  pure $ fromSemVerParts [major, minor]

toSemVerVersion :: RestylerVersion -> Maybe SemVer.Version
toSemVerVersion =
  hush
    . SemVer.fromText
    <=< T.stripPrefix "v"
    . unRestylerVersion

fromSemVerParts :: [Int] -> Text
fromSemVerParts = ("v" <>) . T.intercalate "." . map (pack . show)
