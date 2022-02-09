{-# LANGUAGE RecordWildCards #-}


module TiredPixel.Common.Isoxya.Streamer (
    Streamer(..),
    ) where


import           Data.Aeson
import           Data.Time.Clock
import           TiredPixel.Common.URI


data Streamer = Streamer
                  { streamerCrawlBegan    :: UTCTime
                  , streamerCrawlHref     :: Text
                  , streamerData          :: Value
                  , streamerProcessorHref :: Text
                  , streamerProcessorTag  :: Text
                  , streamerRetrieved     :: UTCTime
                  , streamerSiteHref      :: Text
                  , streamerSiteURL       :: URIAbsolute
                  , streamerURL           :: URIAbsolute
                  }
  deriving (Show)
instance FromJSON Streamer where
    parseJSON = withObject "streamer" $ \j -> do
        fCrawl                <- j          .: "crawl"
        fProcessor            <- j          .: "processor"
        fSite                 <- j          .: "site"
        streamerCrawlBegan    <- fCrawl     .: "began"
        streamerCrawlHref     <- fCrawl     .: "href"
        streamerData          <- j          .: "data"
        streamerProcessorHref <- fProcessor .: "href"
        streamerProcessorTag  <- fProcessor .: "tag"
        streamerRetrieved     <- j          .: "retrieved"
        streamerSiteHref      <- fSite      .: "href"
        streamerSiteURL       <- fSite      .: "url"
        streamerURL           <- j          .: "url"
        return Streamer{..}
instance ToJSON Streamer where
    toJSON Streamer{..} = object [
        "crawl"     .= object [
            "began" .= streamerCrawlBegan,
            "href"  .= streamerCrawlHref],
        "data"      .= streamerData,
        "processor" .= object [
            "href"  .= streamerProcessorHref,
            "tag"   .= streamerProcessorTag],
        "retrieved" .= streamerRetrieved,
        "site"      .= object [
            "href"  .= streamerSiteHref,
            "url"   .= streamerSiteURL],
        "url"       .= streamerURL]
