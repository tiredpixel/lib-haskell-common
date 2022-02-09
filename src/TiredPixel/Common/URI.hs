{-# OPTIONS_GHC -fno-warn-orphans #-}


module TiredPixel.Common.URI (
    URIAbsolute(..),
    URIPage(..),
    URIReference(..),
    URIRelative(..),
    URISite(..),
    reqURIPage,
    reqURISite,
    ) where


import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char             as C
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.URI           as URI
import qualified Text.Regex            as R


newtype URIAbsolute = URIAbsolute { unURIAbsolute :: URI.URI }
  deriving (Eq, Ord, Show)
instance FromJSON URIAbsolute where
    parseJSON = withText "URIAbsolute" $
        maybe (fail "invalid URIAbsolute") (pure . URIAbsolute) .
        URI.parseAbsoluteURI . toString
instance ToJSON URIAbsolute where
    toJSON = toJSON . unURIAbsolute

newtype URIPage = URIPage { unURIPage :: URI.URI }
  deriving (Eq, Ord, Show)
instance FromJSON URIPage where
    parseJSON = withText "URIPage" $
        maybe (fail "invalid URIPage") (pure . URIPage) .
        parsePageURI . URI.parseRelativeReference . toString
instance ToJSON URIPage where
    toJSON = toJSON . unURIPage

newtype URIReference = URIReference { unURIReference :: URI.URI }
  deriving (Eq, Ord, Show)
instance FromJSON URIReference where
    parseJSON = withText "URIReference" $
        maybe (fail "invalid URIReference") (pure . URIReference) .
        URI.parseURIReference . toString
instance ToJSON URIReference where
    toJSON = toJSON . unURIReference

newtype URIRelative = URIRelative { unURIRelative :: URI.URI }
  deriving (Eq, Ord, Show)
instance FromJSON URIRelative where
    parseJSON = withText "URIRelative" $
        maybe (fail "invalid URIRelative") (pure . URIRelative) .
        URI.parseRelativeReference . toString
instance ToJSON URIRelative where
    toJSON = toJSON . unURIRelative

newtype URISite = URISite { unURISite :: URI.URI }
  deriving (Eq, Ord, Show)
instance FromJSON URISite where
    parseJSON = withText "URISite" $
        maybe (fail "invalid URISite") (pure . URISite) .
        parseSiteURI . URI.parseAbsoluteURI . toString
instance ToJSON URISite where
    toJSON = toJSON . unURISite

reqURIPage :: HTTP.Request -> URI.URI
reqURIPage req = URI.URI {
    URI.uriScheme    = "",
    URI.uriAuthority = Nothing,
    URI.uriPath      = unslash $ C8.unpack $ HTTP.path req,
    URI.uriQuery     = C8.unpack $ HTTP.queryString req,
    URI.uriFragment  = ""}

reqURISite :: HTTP.Request -> URI.URI
reqURISite req = URI.URI {
    URI.uriScheme    = scheme,
    URI.uriAuthority = Just authority,
    URI.uriPath      = "",
    URI.uriQuery     = "",
    URI.uriFragment  = ""}
    where
        scheme = if HTTP.secure req
            then "https:"
            else "http:"
        authority = URI.URIAuth {
            URI.uriUserInfo = "",
            URI.uriRegName  = map C.toLower $ C8.unpack $ HTTP.host req,
            URI.uriPort     = ":" ++ show (HTTP.port req)}


parsePageURI :: Maybe URI.URI -> Maybe URI.URI
parsePageURI url = do
    url' <- url
    reqURIPage <$> (HTTP.parseRequest . show) url'

parseSiteURI :: Maybe URI.URI -> Maybe URI.URI
parseSiteURI url = do
    url' <- url
    reqURISite <$> (HTTP.parseRequest . show) url'

unslash :: String -> String
unslash url = R.subRegex (R.mkRegex "/+") url "/"


-- no FromJSON URI.URI instance! be more specific!
instance ToJSON URI.URI where
    toJSON o = toJSON (show o :: Text)
