{-# LANGUAGE RecordWildCards #-}


module TPX.Com.ISX.PlugStrm (
    PlugStrm(..),
    ) where


import              Data.Aeson
import              Data.Time.Clock                         (UTCTime)
import              TPX.Com.URI


data PlugStrm = PlugStrm {
    plugStrmCrwlHref     :: Text,
    plugStrmCrwlTBegin   :: UTCTime,
    plugStrmSiteHref     :: Text,
    plugStrmSiteURL      :: URIAbsolute,
    plugStrmOrgHref      :: Text,
    plugStrmPlugProcHref :: Text,
    plugStrmPlugProcTag  :: Text,
    plugStrmURL          :: URIAbsolute,
    plugStrmTRetrieval   :: UTCTime,
    plugStrmData         :: Value
    } deriving (Show)
instance FromJSON PlugStrm where
    parseJSON = withObject "plug_strm" $ \j -> do
        fCrwl <- j .: "crwl"
        plugStrmCrwlHref   <- fCrwl .: "href"
        plugStrmCrwlTBegin <- fCrwl .: "t_begin"
        fSite <- j .: "site"
        plugStrmSiteHref <- fSite .: "href"
        plugStrmSiteURL  <- fSite .: "url"
        fOrg <- j .: "org"
        plugStrmOrgHref <- fOrg .: "href"
        fPlugProc <- j .: "plug_proc"
        plugStrmPlugProcHref <- fPlugProc .: "href"
        plugStrmPlugProcTag  <- fPlugProc .: "tag"
        plugStrmURL <- j .: "url"
        plugStrmTRetrieval <- j .: "t_retrieval"
        plugStrmData <- j .: "data"
        return PlugStrm{..}
instance ToJSON PlugStrm where
    toJSON PlugStrm{..} = object [
        "crwl" .= object [
            "href"    .= plugStrmCrwlHref,
            "t_begin" .= plugStrmCrwlTBegin],
        "site" .= object [
            "href" .= plugStrmSiteHref,
            "url"  .= plugStrmSiteURL],
        "org" .= object [
            "href" .= plugStrmOrgHref],
        "plug_proc" .= object [
            "href" .= plugStrmPlugProcHref,
            "tag"  .= plugStrmPlugProcTag],
        "url" .= plugStrmURL,
        "t_retrieval" .= plugStrmTRetrieval,
        "data" .= plugStrmData]
