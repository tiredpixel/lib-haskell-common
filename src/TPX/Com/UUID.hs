module TPX.Com.UUID (
    fromByteStringNil,
    generateUUID,
    ) where


import Data.UUID
import Data.UUID.V4


fromByteStringNil :: ByteString -> UUID
fromByteStringNil = fromMaybe nil . fromASCIIBytes

generateUUID :: MonadIO m => m UUID
generateUUID = liftIO nextRandom
