module TPX.Com.API.Req (
    ValidateJSON(..),
    getBoundedJSON',
    getJSON',
    ) where


import              Data.Aeson
import              Snap.Core
import              Snap.Extras.JSON
import qualified    TPX.Com.API.Resource                    as  R


class ValidateJSON r where
    validateJSON :: Either Text r -> Snap (Either R.ErrorN r)
    validateJSON (Right r)  = validateJSONOk r
    validateJSON (Left err) = validateJSONErr err
    
    validateJSONOk :: r -> Snap (Either R.ErrorN r)
    validateJSONOk r = return $ Right r
    
    validateJSONErr :: Text -> Snap (Either R.ErrorN r)
    validateJSONErr err = return $ Left R.ErrorN {
        R.errorNDebug = err}

getBoundedJSON' :: (MonadSnap m, FromJSON a) => Int64 -> m (Either Text a)
getBoundedJSON' s = do
    v <- getBoundedJSON s
    return $ case v of
        Left l  -> Left $ toText l
        Right r -> Right r

getJSON' :: (MonadSnap m, FromJSON a) => m (Either Text a)
getJSON' = do
    v <- getJSON
    return $ case v of
        Left l  -> Left $ toText l
        Right r -> Right r
