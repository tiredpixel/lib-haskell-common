module TPX.Com.Net (
    Conn,
    jsonNDReq,
    makeReq,
    makeRes,
    openConn,
    ) where


import              Network.HTTP.Conduit
import              Network.HTTP.Types
import              Network.URI                             hiding (path)


type Conn = Manager

jsonNDReq :: Request -> Request
jsonNDReq req = req {
    requestHeaders =
        ("Content-Type", "application/x-ndjson") : requestHeaders req }

makeReq :: ByteString -> URI -> LByteString -> Request
makeReq verb url body = setRequest verb body $ parseRequest_ $ show' url

makeRes :: Request -> Conn -> IO (Response LByteString)
makeRes = httpLbs

openConn :: IO Manager
openConn = newManager tlsManagerSettings


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
