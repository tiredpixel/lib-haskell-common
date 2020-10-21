module TPX.Com.API.Test (
    module E,
    assertElemN,
    assertNoContent,
    getLink,
    linkQValue,
    logReq ,
    logReq',
    logRes,
    postJSON,
    shouldContainSubseq,
    testResources
    ) where


import              Control.Lens                            as  E
import              Data.Aeson                              as  E   hiding  ((.=))
import              Data.Aeson.Lens                         as  E
import              Data.List                               (elemIndex, isSubsequenceOf)
import              Network.URI                             (URI)
import              Snap.Core                               as  E   hiding  (addHeader, setContentType, setHeader)
import              Snap.Test                               as  E
import              System.Directory
import              System.FilePath                         as  FP
import              TPX.Com.API.Aeson                       as  E
import              Test.Hspec                              as  E
import qualified    Data.ByteString                         as  BS
import qualified    Data.ByteString.Char8                   as  C8
import qualified    Network.HTTP.Link                       as  Link
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
        Link.Link linkF [(Link.Rel, "first")],
        Link.Link linkN [(Link.Rel, "next")],
        Link.Link linkP [(Link.Rel, "prev")]
        ] <- Link.parseLinkHeader link
    Just (c (linkF :: URI), c (linkN :: URI), c (linkP :: URI))
    where
        c = show

linkQValue :: ByteString -> ByteString -> ByteString
linkQValue p = BS.drop 1 .
    snd . BS.breakSubstring "=" . snd . BS.breakSubstring p

logReq :: FilePath -> RequestBuilder IO () -> IO ()
logReq tag req = do
    req' <- buildRequest req
    req'' <- requestToString req'
    logReq' tag req''

logReq' :: FilePath -> ByteString -> IO ()
logReq' tag req = do
    createDirectoryIfMissing True $ takeDirectory f
    let (header, body) = splitLog req
    writeFileBS (f FP.<.> logExtH) header
    writeFileBS (f FP.<.> logExtB) body
    where
        f = logDir </> tag

logRes :: FilePath -> Response -> IO ()
logRes tag res = do
    createDirectoryIfMissing True $ takeDirectory f
    -- HACK: show res not responseToString, as that doubles body
    -- https://github.com/snapframework/snap-core/issues/233#issuecomment-258780775
    let (header, body) = splitLog $ show res
    writeFileBS (f FP.<.> logExtH) header
    writeFileBS (f FP.<.> logExtB) body
    where
        f = logDir </> tag

postJSON :: (MonadIO m, ToJSON a) => ByteString -> a -> RequestBuilder m ()
postJSON u p = postRaw u "application/json" $ toStrict $ encode p

shouldContainSubseq :: Eq a => [a] -> [a] -> Expectation
shouldContainSubseq a b = (b `isSubsequenceOf` a) `shouldBe` True

testResources :: Eq t => Response -> [t] ->
    (Value -> Maybe t) -> (t -> Value -> IO ()) -> [Int] -> IO ()
testResources res rIds h t es = do
    b <- getResponseBody res
    let rIdsB = catMaybes $ h <$> b ^.. values
    catMaybes (flip elemIndex rIds <$> rIdsB) `shouldBe` es
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
