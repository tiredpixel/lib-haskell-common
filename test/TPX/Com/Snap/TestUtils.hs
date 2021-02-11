module TPX.Com.Snap.TestUtils (
    assertElemN,
    assertNoContent,
    getLink,
    linkQValue,
    logReq ,
    logReq',
    logRes,
    postJSON,
    shouldContainSubseq,
    testResources,
    ) where


import              Control.Lens                            hiding ((<.>))
import              Data.Aeson
import              Data.Aeson.Lens
import              Network.URI
import              Prelude                                 hiding (put)
import              Snap.Core                               hiding (addHeader, setContentType, setHeader)
import              Snap.Test
import              System.Directory
import              System.FilePath
import              Test.Hspec
import qualified    Data.ByteString                         as  B
import qualified    Data.ByteString.Char8                   as  C8
import qualified    Data.List                               as  L
import qualified    Network.HTTP.Link                       as  HTTP
import qualified    Relude.Unsafe                           as  Unsafe


assertElemN :: Response -> Int -> IO ()
assertElemN res n = do
    b <- getResponseBody res
    length (b ^. _Object) `shouldBe` n

assertNoContent :: Response -> IO ()
assertNoContent res = do
    rspStatus res `shouldBe` 204
    assertElemN res 0

getLink :: HasHeaders a => a -> Maybe (ByteString, ByteString, ByteString)
getLink res = do
    link <- decodeUtf8 <$> getHeader "Link" res
    [
        HTTP.Link linkF [(HTTP.Rel, "first")],
        HTTP.Link linkN [(HTTP.Rel, "next")],
        HTTP.Link linkP [(HTTP.Rel, "prev")]
        ] <- HTTP.parseLinkHeader link
    Just (c (linkF :: URI), c (linkN :: URI), c (linkP :: URI))
    where
        c = show

linkQValue :: ByteString -> ByteString -> ByteString
linkQValue p = B.drop 1 .
    snd . B.breakSubstring "=" . snd . B.breakSubstring p

logReq :: FilePath -> RequestBuilder IO () -> IO ()
logReq tag req = do
    req' <- buildRequest req
    req'' <- requestToString req'
    logReq' tag req''

logReq' :: FilePath -> ByteString -> IO ()
logReq' tag req = do
    createDirectoryIfMissing True $ takeDirectory f
    let (header, body) = splitLog req
    writeFileBS (f <.> logExtH) header
    writeFileBS (f <.> logExtB) body
    where
        f = logDir </> tag

logRes :: FilePath -> Response -> IO ()
logRes tag res = do
    createDirectoryIfMissing True $ takeDirectory f
    -- HACK: show res not responseToString, as that doubles body
    -- https://github.com/snapframework/snap-core/issues/233#issuecomment-258780775
    let (header, body) = splitLog $ show res
    writeFileBS (f <.> logExtH) header
    writeFileBS (f <.> logExtB) body
    where
        f = logDir </> tag

postJSON :: (MonadIO m, ToJSON a) => ByteString -> a -> RequestBuilder m ()
postJSON u p = postRaw u "application/json" $ toStrict $ encode p

shouldContainSubseq :: Eq a => [a] -> [a] -> Expectation
shouldContainSubseq a b = (b `L.isSubsequenceOf` a) `shouldBe` True

testResources :: Eq t => Response -> [t] ->
    (Value -> Maybe t) -> (t -> Value -> IO ()) -> [Int] -> IO ()
testResources res rIds h t es = do
    b <- getResponseBody res
    let rIdsB = catMaybes $ h <$> b ^.. values
    catMaybes (flip L.elemIndex rIds <$> rIdsB) `shouldBe` es
    for_ (zip [0..] es) $ \(i,j) -> t (rIds Unsafe.!! j) (b ^?! nth i)


logDir :: FilePath
logDir = "test/log"

logExtH :: FilePath
logExtH = ".header"

logExtB :: FilePath
logExtB = ".body"

splitLog :: ByteString -> (ByteString, ByteString)
splitLog log = (header, body')
    where
        sep = "\r\n\r\n"
        (header, body) = C8.breakSubstring sep log
        body' = fromMaybe "" $ C8.stripPrefix sep body
