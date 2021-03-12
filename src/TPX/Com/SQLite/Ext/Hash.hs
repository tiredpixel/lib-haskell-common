{-# OPTIONS_GHC -fno-warn-orphans #-}


module TPX.Com.SQLite.Ext.Hash () where


import           Crypto.Hash
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.ToField
import qualified Data.ByteArray                   as BA
import qualified Data.ByteString                  as B


instance (HashAlgorithm a, Typeable a) => FromField (Digest a) where
    fromField f@(Field (SQLBlob d) _) = case digestFromByteString d of
        Just d' -> return d'
        Nothing -> returnError ConversionFailed f "must be Digest"
    fromField f = returnError ConversionFailed f "must be SQLBlob"
instance ToField (Digest a) where
    toField = toField . SQLBlob . B.pack . BA.unpack
