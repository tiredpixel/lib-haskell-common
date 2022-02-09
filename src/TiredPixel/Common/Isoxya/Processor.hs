{-# LANGUAGE RecordWildCards #-}


module TiredPixel.Common.Isoxya.Processor (
    ProcessorI(..),
    ProcessorIBody,
    ProcessorIHeader,
    ProcessorIMeta(..),
    ProcessorO(..),
    ProcessorOData,
    ProcessorOURL,
    ) where


import           Data.Aeson
import qualified Data.ByteString.Base64 as B64
import           Data.Fixed             (Pico)
import           TiredPixel.Common.URI


data ProcessorI = ProcessorI
                    { processorIMeta   :: ProcessorIMeta
                    , processorIHeader :: ProcessorIHeader
                    , processorIBody   :: ProcessorIBody
                    }
  deriving (Show)
instance FromJSON ProcessorI where
    parseJSON = withObject "processor_i" $ \j -> do
        fMeta                  <- j     .:  "meta"
        fBody                  <- j     .:  "body"
        processorIMetaConfig   <- fMeta .:? "config"
        processorIMetaDuration <- fMeta .:? "duration"
        processorIMetaError    <- fMeta .:? "error"
        processorIMetaMethod   <- fMeta .:  "method"
        processorIMetaStatus   <- fMeta .:? "status"
        processorIMetaURL      <- fMeta .:  "url"
        processorIHeader       <- j     .:  "header"
        let processorIBody = B64.decodeLenient $ encodeUtf8 (fBody :: Text)
        let processorIMeta = ProcessorIMeta{..}
        return ProcessorI{..}
instance ToJSON ProcessorI where
    toJSON ProcessorI{..} = object [
        "meta"   .= processorIMeta,
        "header" .= processorIHeader,
        "body"   .= (decodeUtf8 (B64.encode processorIBody) :: Text)]

type ProcessorIBody = ByteString

type ProcessorIHeader = Map Text Text

data ProcessorIMeta = ProcessorIMeta
                        { processorIMetaConfig   :: Maybe Value
                        , processorIMetaDuration :: Maybe Pico
                        , processorIMetaError    :: Maybe Text
                        , processorIMetaMethod   :: Text
                        , processorIMetaStatus   :: Maybe Integer
                        , processorIMetaURL      :: URIAbsolute
                        }
  deriving (Show)
instance ToJSON ProcessorIMeta where
    toJSON ProcessorIMeta{..} = object [
        "config"   .= processorIMetaConfig,
        "duration" .= processorIMetaDuration,
        "error"    .= processorIMetaError,
        "method"   .= processorIMetaMethod,
        "status"   .= processorIMetaStatus,
        "url"      .= processorIMetaURL]

data ProcessorO = ProcessorO
                    { processorOData :: ProcessorOData
                    , processorOURLs :: Set ProcessorOURL
                    }
  deriving (Show)
instance FromJSON ProcessorO where
    parseJSON = withObject "processor_o" $ \j -> do
        processorOData <- j .: "data"
        processorOURLs <- j .: "urls"
        return ProcessorO{..}
instance ToJSON ProcessorO where
    toJSON o = object [
        "data" .= processorOData o,
        "urls" .= processorOURLs o]

type ProcessorOData = Value

type ProcessorOURL = URIReference
