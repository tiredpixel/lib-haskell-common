module TPX.Com.API.Test (
    module E,
    assertElemN,
    assertNoContent,
    logReq ,
    logReq',
    logRes,
    postJSON,
    shouldContainSubseq
    ) where


import              Control.Lens                            as  E
import              Data.Aeson                              as  E   hiding  ((.=))
import              Data.Aeson.Lens                         as  E
import              Data.List                               (isSubsequenceOf)
import              Snap.Core                               as  E   hiding  (addHeader, setContentType, setHeader)
import              Snap.Test                               as  E
import              System.Directory
import              System.FilePath                         as  FP
import              TPX.Com.API.Aeson                       as  E
import              Test.Hspec                              as  E
import qualified    Data.ByteString.Char8                   as  C8


assertElemN :: Response -> Int -> IO ()
assertElemN res n = do
    b <- getResponseBody res
    length (b ^. _Object) `shouldBe` n

assertNoContent :: Response -> IO ()
assertNoContent res = do
    rspStatus res `shouldBe` 204
    assertElemN res 0

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
