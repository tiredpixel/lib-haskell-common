{-# LANGUAGE RecordWildCards #-}


module TPX.Com.Isoxya.PlugProc (
    PlugProcI(..),
    PlugProcIBody,
    PlugProcIHeader,
    PlugProcIMeta(..),
    PlugProcO(..),
    PlugProcOData,
    PlugProcOURL,
    ) where


import           Data.Aeson
import           TPX.Com.URI
import qualified Data.ByteString.Base64 as B64


data PlugProcI = PlugProcI {
    plugProcIMeta   :: PlugProcIMeta,
    plugProcIHeader :: PlugProcIHeader,
    plugProcIBody   :: PlugProcIBody
    } deriving (Show)
instance FromJSON PlugProcI where
    parseJSON = withObject "plug_proc_i" $ \j -> do
        fMeta <- j .: "meta"
        plugProcIMetaURL      <- fMeta .: "url"
        plugProcIMetaMethod   <- fMeta .: "method"
        plugProcIMetaStatus   <- fMeta .:? "status"
        plugProcIMetaDuration <- fMeta .:? "duration"
        plugProcIMetaErr      <- fMeta .:? "err"
        plugProcIMetaConfig   <- fMeta .:? "config"
        let plugProcIMeta = PlugProcIMeta{..}
        plugProcIHeader <- j .: "header"
        fBody <- j .: "body"
        let plugProcIBody = B64.decodeLenient $ encodeUtf8 (fBody :: Text)
        return PlugProcI{..}
instance ToJSON PlugProcI where
    toJSON PlugProcI{..} = object [
        "meta"   .= plugProcIMeta,
        "header" .= plugProcIHeader,
        "body"   .= (decodeUtf8 (B64.encode plugProcIBody) :: Text)]

type PlugProcIBody = ByteString

type PlugProcIHeader = Map Text Text

data PlugProcIMeta = PlugProcIMeta {
    plugProcIMetaURL      :: URIAbsolute,
    plugProcIMetaMethod   :: Text,
    plugProcIMetaStatus   :: Maybe Integer,
    plugProcIMetaDuration :: Maybe Rational,
    plugProcIMetaErr      :: Maybe Text,
    plugProcIMetaConfig   :: Maybe Value
    } deriving (Show)
instance ToJSON PlugProcIMeta where
    toJSON PlugProcIMeta{..} = object [
        "url"      .= plugProcIMetaURL,
        "method"   .= plugProcIMetaMethod,
        "status"   .= plugProcIMetaStatus,
        "duration" .= plugProcIMetaDuration,
        "err"      .= plugProcIMetaErr,
        "config"   .= plugProcIMetaConfig]

data PlugProcO = PlugProcO {
    plugProcOData :: PlugProcOData,
    plugProcOURLs :: Set PlugProcOURL
    } deriving (Show)
instance FromJSON PlugProcO where
    parseJSON = withObject "plug_proc_o" $ \j -> do
        plugProcOData <- j .: "data"
        plugProcOURLs <- j .: "urls"
        return PlugProcO{..}
instance ToJSON PlugProcO where
    toJSON o = object [
        "data" .= plugProcOData o,
        "urls" .= plugProcOURLs o]

type PlugProcOData = Value

type PlugProcOURL = URIReference
