{-# OPTIONS_GHC -fno-warn-orphans #-}


module TPX.Com.API.Ext.URI (
    URIAbsolute(..),
    URIReference(..),
    URIRelative(..)
) where


import              Data.Aeson
import              Network.URI


newtype URIAbsolute = URIAbsolute { unURIAbsolute :: URI
    } deriving (Show, Eq, Ord)
instance FromJSON URIAbsolute where
    parseJSON = withText "URIAbsolute" $
        maybe (fail "invalid URIAbsolute") (pure . URIAbsolute) .
        parseAbsoluteURI . toString
instance ToJSON URIAbsolute where
    toJSON = toJSON . unURIAbsolute

newtype URIReference = URIReference { unURIReference :: URI
    } deriving (Show, Eq, Ord)
instance FromJSON URIReference where
    parseJSON = withText "URIReference" $
        maybe (fail "invalid URIReference") (pure . URIReference) .
        parseURIReference . toString
instance ToJSON URIReference where
    toJSON = toJSON . unURIReference

newtype URIRelative = URIRelative { unURIRelative :: URI
    } deriving (Show, Eq, Ord)
instance FromJSON URIRelative where
    parseJSON = withText "URIRelative" $
        maybe (fail "invalid URIRelative") (pure . URIRelative) .
        parseRelativeReference . toString
instance ToJSON URIRelative where
    toJSON = toJSON . unURIRelative


-- no FromJSON URI instance! be more specific!
instance ToJSON URI where
    toJSON o = toJSON (show o :: Text)
