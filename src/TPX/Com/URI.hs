{-# OPTIONS_GHC -fno-warn-orphans #-}


module TPX.Com.URI (
    URIAbsolute(..),
    URIReference(..),
    URIRelative(..),
    ) where


import              Data.Aeson
import qualified    Network.URI                             as  URI


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


-- no FromJSON URI.URI instance! be more specific!
instance ToJSON URI.URI where
    toJSON o = toJSON (show o :: Text)
