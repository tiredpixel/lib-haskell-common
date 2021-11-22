{-# OPTIONS_GHC -fno-warn-orphans #-}


module TiredPixel.Common.URI (
    URIAbsolute(..),
    URIReference(..),
    URIRelative(..),
    reqURIPage,
    reqURISite,
    ) where


import           Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.Char             as C
import qualified Network.HTTP.Conduit  as HTTP
import qualified Network.URI           as URI
import qualified Text.Regex            as R


newtype URIAbsolute = URIAbsolute { unURIAbsolute :: URI.URI
    } deriving (Show, Eq, Ord)
instance FromJSON URIAbsolute where
    parseJSON = withText "URIAbsolute" $
        maybe (fail "invalid URIAbsolute") (pure . URIAbsolute) .
        URI.parseAbsoluteURI . toString
instance ToJSON URIAbsolute where
    toJSON = toJSON . unURIAbsolute

newtype URIReference = URIReference { unURIReference :: URI.URI
    } deriving (Show, Eq, Ord)
instance FromJSON URIReference where
    parseJSON = withText "URIReference" $
        maybe (fail "invalid URIReference") (pure . URIReference) .
        URI.parseURIReference . toString
instance ToJSON URIReference where
    toJSON = toJSON . unURIReference

newtype URIRelative = URIRelative { unURIRelative :: URI.URI
    } deriving (Show, Eq, Ord)
instance FromJSON URIRelative where
    parseJSON = withText "URIRelative" $
        maybe (fail "invalid URIRelative") (pure . URIRelative) .
        URI.parseRelativeReference . toString
instance ToJSON URIRelative where
    toJSON = toJSON . unURIRelative

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


unslash :: String -> String
unslash url = R.subRegex (R.mkRegex "/+") url "/"


-- no FromJSON URI.URI instance! be more specific!
instance ToJSON URI.URI where
    toJSON o = toJSON (show o :: Text)
