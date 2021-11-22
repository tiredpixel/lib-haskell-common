{-# OPTIONS_GHC -fno-warn-orphans #-}


module TiredPixel.Common.SQLite.Ext.UUID () where


import Data.UUID
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.ToField


instance FromField UUID where
    fromField f@(Field (SQLBlob d) _) = case fromASCIIBytes d of
        Just d' -> return d'
        Nothing -> returnError ConversionFailed f "must be UUID"
    fromField f = returnError ConversionFailed f "must be SQLBlob"
instance ToField UUID where
    toField = toField . SQLBlob . toASCIIBytes
