module TPX.Com.Net (
    Conn,
    extractResRobots,
    jsonNDReq,
    jsonReq,
    makeReq ,
    makeReq',
    makeRes,
    makeResLim,
    openConn,
    robotsReq,
    userAgentReq,
    ) where


import           Control.Exception            (throw)
import           Control.Monad.Trans.Resource (runResourceT)
import           Data.Conduit                 (runConduit, (.|))
import           Network.HTTP.Conduit
import           Network.HTTP.Types
import           Network.URI                  hiding (path)
import qualified Data.Conduit.Binary          as CB


type Conn = Manager

extractResRobots :: Response LByteString -> LByteString
extractResRobots res = case statusCode $ responseStatus res of
        200 -> responseBody res
        404 -> allow
        _   -> disallow
    where
        allow    = "User-agent: *\nAllow: *"
        disallow = "User-agent: *\nDisallow: *"

jsonNDReq :: Request -> Request
jsonNDReq req = req {
    requestHeaders =
        ("Content-Type", "application/x-ndjson") : requestHeaders req }

jsonReq :: Request -> Request
jsonReq req = req {
    requestHeaders =
        ("Content-Type", "application/json") : requestHeaders req }

makeReq :: ByteString -> URI -> LByteString -> Request
makeReq verb url body = setRequest verb body $ parseRequest_ $ show' url

makeReq' :: ByteString -> URI -> LByteString -> Request
makeReq' verb url body = setRequest verb body $ parseUrlThrow_ $ show' url

makeRes :: MonadIO m => Request -> Conn -> m (Response LByteString)
makeRes = httpLbs

makeResLim :: MonadIO m => Int -> Request -> Conn -> m (Response LByteString)
makeResLim s req n = liftIO $ runResourceT $ do
    res <- http req n
    bss <- runConduit $ responseBody res .| CB.take s
    return res {
        responseBody = bss}

openConn :: IO Manager
openConn = newManager tlsManagerSettings

robotsReq :: Request -> Request
robotsReq req = req {
    method        = "GET",
    path          = "/robots.txt",
    queryString   = "",
    redirectCount = 5}

userAgentReq :: ByteString -> Request -> Request
userAgentReq ua req = req {
    requestHeaders = ("User-Agent", ua) : requestHeaders req }


{-# ANN  parseUrlThrow_ ("HLint: ignore Use impureThrow" :: String) #-}
parseUrlThrow_ :: String -> Request
parseUrlThrow_ = either throw id . parseUrlThrow

setRequest :: ByteString -> LByteString -> Request -> Request
setRequest verb body req = req {
    method          = verb,
    requestBody     = RequestBodyLBS body,
    redirectCount   = 0,
    requestVersion  = http11,
    responseTimeout = responseTimeoutMicro timeout}
    where
        timeout = 30000000 -- 30 s

show' :: URI -> String
show' u = uriToString id u ""
