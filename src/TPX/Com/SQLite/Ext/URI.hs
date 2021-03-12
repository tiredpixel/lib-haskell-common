{-# OPTIONS_GHC -fno-warn-orphans #-}


module TPX.Com.SQLite.Ext.URI () where


import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.ToField
import Database.SQLite3
import Network.URI


instance FromField URI where
    fromField f@(Field (SQLText d) _) = case parseURIReference (toString d) of
        Just d' -> return d'
        Nothing -> returnError ConversionFailed f "must be URIReference"
    fromField f = returnError ConversionFailed f "must be SQLText"
instance ToField URI where
    toField u = toField (show u :: Text)
