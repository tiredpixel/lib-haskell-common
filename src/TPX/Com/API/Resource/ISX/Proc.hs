module TPX.Com.API.Resource.ISX.Proc (
    ProcI(..),
    ProcIBody,
    ProcIHeader,
    ProcIMeta(..),
    ProcO(..),
    ProcOData,
    ProcOUrl
    ) where


import              Data.Aeson
import              TPX.Com.API.Ext.URI
import qualified    Data.ByteString.Base64                  as  B64


data ProcI = ProcI {
    procIMeta   :: ProcIMeta,
    procIHeader :: ProcIHeader,
    procIBody   :: ProcIBody
    } deriving (Show)
instance FromJSON ProcI where
    parseJSON = withObject "ProcI" $ \j -> do
        fMeta           <- j .: "meta"
        fMetaUrl        <- fMeta .: "url"
        fMetaStatusCode <- fMeta .:? "status_code"
        fMetaConfig     <- fMeta .:? "config"
        fHeader         <- j .: "header"
        fBody           <- j .: "body"
        let meta = ProcIMeta fMetaUrl fMetaStatusCode fMetaConfig
        let body = B64.decodeLenient $ encodeUtf8 (fBody :: Text)
        return $ ProcI meta fHeader body
instance ToJSON ProcI where
    toJSON o = object [
        "meta"   .= object [
            "url"         .= procIMetaUrl (procIMeta o),
            "status_code" .= procIMetaStatusCode (procIMeta o),
            "config"      .= procIMetaConfig (procIMeta o)],
        "header" .= procIHeader o,
        "body"   .= ((decodeUtf8 $ B64.encode $ procIBody o) :: Text)]

type ProcIBody = ByteString

type ProcIHeader = Map Text Text

data ProcIMeta = ProcIMeta {
    procIMetaUrl        :: URIAbsolute,
    procIMetaStatusCode :: Maybe Integer,
    procIMetaConfig     :: Maybe Value
    } deriving (Show)

data ProcO = ProcO {
    procOData :: ProcOData,
    procOUrls :: Set ProcOUrl
    } deriving (Show)
instance FromJSON ProcO where
    parseJSON = withObject "ProcO" $ \j -> do
        fData <- j .: "data"
        fUrls <- j .: "urls"
        return $ ProcO fData fUrls
instance ToJSON ProcO where
    toJSON o = object [
        "data" .= procOData o,
        "urls" .= procOUrls o]

type ProcOData = Value

type ProcOUrl = URIReference
