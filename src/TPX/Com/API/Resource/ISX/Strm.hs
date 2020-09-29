module TPX.Com.API.Resource.ISX.Strm (
    Strm(..)
    ) where


import              Data.Aeson
import              Data.Time.Clock                         (UTCTime)
import              TPX.Com.API.Ext.URI


data Strm = Strm {
    strmData           :: Value,
    strmOrgHref        :: Text,
    strmOrgProcHref    :: Text,
    strmOrgProcTag     :: Text,
    strmSiteHref       :: Text,
    strmSiteUrl        :: URIAbsolute,
    strmSiteSnapHref   :: Text,
    strmSiteSnapTBegin :: UTCTime,
    strmTRetrieval     :: UTCTime,
    strmUrl            :: URIAbsolute
    } deriving (Show)
instance FromJSON Strm where
    parseJSON = withObject "Strm" $ \j -> do
        fData           <- j .: "data"
        fOrg            <- j .: "org"
        fOrgHref        <- fOrg .: "href"
        fOrgProc        <- j .: "org_proc"
        fOrgProcHref    <- fOrgProc .: "href"
        fOrgProcTag     <- fOrgProc .: "tag"
        fSite           <- j .: "site"
        fSiteHref       <- fSite .: "href"
        fSiteUrl        <- fSite .: "url"
        fSiteSnap       <- j .: "site_snap"
        fSiteSnapHref   <- fSiteSnap .: "href"
        fSiteSnapTBegin <- fSiteSnap .: "t_begin"
        fTRetrieval     <- j .: "t_retrieval"
        fUrl            <- j .: "url"
        return $ Strm fData fOrgHref fOrgProcHref fOrgProcTag
            fSiteHref fSiteUrl fSiteSnapHref fSiteSnapTBegin fTRetrieval fUrl
instance ToJSON Strm where
    toJSON o = object [
        "data"        .= strmData o,
        "org"         .= object [
            "href" .= strmOrgHref o],
        "org_proc"    .= object [
            "href" .= strmOrgProcHref o,
            "tag"  .= strmOrgProcTag o],
        "site"        .= object [
            "href" .= strmSiteHref o,
            "url"  .= strmSiteUrl o],
        "site_snap"   .= object [
            "href"    .= strmSiteSnapHref o,
            "t_begin" .= strmSiteSnapTBegin o],
        "t_retrieval" .= strmTRetrieval o,
        "url"         .= strmUrl o]
