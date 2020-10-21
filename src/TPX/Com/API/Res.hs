module TPX.Com.API.Res (
    badReq,
    created,
    intErr ,
    intErr',
    noContent,
    notFound,
    run,
    runValidate
    ) where


import              Data.Aeson.Types                        (ToJSON)
import              Snap.Core
import              Snap.Extras.JSON
import qualified    System.Posix.Signals                    as  Sig
import qualified    TPX.Com.API.Resource.CommonError        as  RC


badReq :: RC.ErrorN -> Snap ()
badReq err = do
    modifyResponse $ setResponseCode 400
    writeJSON err
    res <- getResponse
    finishWith res

created :: (ToJSON a) => ByteString -> a -> Snap ()
created loc msg = do
    modifyResponse $ setResponseCode 201
    modifyResponse $ setHeader "Location" loc
    writeJSON msg

intErr :: SomeException -> Snap ()
intErr ex = do
    logError $ encodeUtf8 (show ex :: Text)
    modifyResponse $ setResponseCode 500

intErr' :: SomeException -> Snap ()
intErr' ex = do
    intErr ex
    liftIO $ Sig.raiseSignal Sig.sigINT

noContent :: Snap ()
noContent = modifyResponse $ setResponseCode 204

notFound :: Snap ()
notFound = do
    modifyResponse $ setResponseCode 404
    res <- getResponse
    finishWith res

run :: Snap a1 -> MaybeT Snap a2 -> Snap (Maybe a2)
run f e = do
    r_ <- runMaybeT e
    case r_ of
        Just r  -> return $ Just r
        Nothing -> f >> return Nothing

runValidate :: Either RC.ErrorN a -> Snap (Maybe a)
runValidate e = case e of
    Right r  -> return $ Just r
    Left err -> f err >> return Nothing
    where
        f = badReq
