module TiredPixel.Common.UUID (
    fromByteStringNil,
    genUUID,
    ) where


import Data.UUID
import Data.UUID.V4


fromByteStringNil :: ByteString -> UUID
fromByteStringNil = fromMaybe nil . fromASCIIBytes

genUUID :: MonadIO m => m UUID
genUUID = liftIO nextRandom
