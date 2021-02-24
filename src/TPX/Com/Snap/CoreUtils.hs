{-# LANGUAGE MultiParamTypeClasses #-}


module TPX.Com.Snap.CoreUtils (
    ErrorC(..),
    ValidateJSON(..),
    badReq,
    created,
    getBoundedJSON',
    getJSON',
    intErr ,
    intErr',
    mergeObject,
    noContent,
    notFound,
    run,
    runValidate,
    snapCfg,
    ) where


import           Data.Aeson
import           Snap.Core
import           Snap.Extras.JSON
import           Snap.Http.Server.Config
import           System.Posix
import qualified Data.HashMap.Strict     as HM


newtype ErrorC = ErrorC { errorCDebug :: Text
    } deriving (Show)
instance ToJSON ErrorC where
    toJSON o = object [
        "msg" .= errorCDebug o]

class ValidateJSON r where
    validateJSON :: MonadSnap m => Either Text r -> m (Either ErrorC r)
    validateJSON (Right r)  = validateJSONOk r
    validateJSON (Left err) = validateJSONErr err
    
    validateJSONOk :: MonadSnap m => r -> m (Either ErrorC r)
    validateJSONOk r = return $ Right r
    
    validateJSONErr :: MonadSnap m => Text -> m (Either ErrorC r)
    validateJSONErr err = return $ Left ErrorC {
        errorCDebug = err}

badReq :: MonadSnap m => ErrorC -> m ()
badReq err = do
    modifyResponse $ setResponseCode 400
    writeJSON err
    getResponse >>= finishWith

created :: (MonadSnap m, ToJSON a) => ByteString -> a -> m ()
created loc msg = do
    modifyResponse $ setResponseCode 201
    modifyResponse $ setHeader "Location" loc
    writeJSON msg

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

intErr :: MonadSnap m => SomeException -> m ()
intErr ex = do
    logError $ encodeUtf8 (show ex :: Text)
    modifyResponse $ setResponseCode 500

intErr' :: MonadSnap m => SomeException -> m ()
intErr' ex = do
    intErr ex
    liftIO $ raiseSignal sigINT

mergeObject :: Value -> Value -> Value
mergeObject (Object a) (Object b) = Object $ HM.unionWith mergeObject a b
mergeObject _ b = b

noContent :: MonadSnap m => m ()
noContent = modifyResponse $ setResponseCode 204

notFound :: MonadSnap m => m ()
notFound = do
    modifyResponse $ setResponseCode 404
    getResponse >>= finishWith

run :: Monad m => m a1 -> MaybeT m a2 -> m (Maybe a2)
run f e = do
    r_ <- runMaybeT e
    case r_ of
        Just r  -> return $ Just r
        Nothing -> f >> return Nothing

runValidate :: MonadSnap m => Either ErrorC a -> m (Maybe a)
runValidate e = case e of
    Right r  -> return $ Just r
    Left err -> f err >> return Nothing
    where
        f = badReq

snapCfg :: MonadSnap m => Config m a
snapCfg =
    setAccessLog (ConfigFileLog "-") $
    setErrorLog (ConfigFileLog "-") $
    setErrorHandler intErr' defaultConfig
