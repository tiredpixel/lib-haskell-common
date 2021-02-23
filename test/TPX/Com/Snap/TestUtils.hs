module TPX.Com.Snap.TestUtils (
    addHeaderAuth,
    defaultPL1,
    defaultPL2,
    emptyO,
    emptyP,
    getLink,
    getResponseBody,
    insertLink,
    logReq ,
    logReq',
    logRes,
    postJSON,
    putJSON,
    readProcessText,
    runRequest,
    shouldBe,
    shouldContain,
    shouldMeasure,
    shouldNotBe,
    shouldSatisfy,
    ) where


import           Data.Aeson
import           Network.URI
import           Prelude                hiding (put)
import           Snap.Core              hiding (addHeader, setHeader)
import           Snap.Test              hiding (getResponseBody)
import           System.Directory
import           System.FilePath
import           System.Process
import qualified Control.Monad.State    as St
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C8
import qualified Data.HashMap.Strict    as HM
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Network.HTTP.Link      as HTTP
import qualified Snap.Test              as ST
import qualified Test.Hspec.Core.Spec   as Hs
import qualified Test.Hspec.Snap        as Hs


addHeaderAuth :: Monad m => ByteString -> RequestBuilder m ()
addHeaderAuth b = addHeader "Authorization" $ "Basic " <> B64.encode b

defaultPL1 :: Params
defaultPL1 = M.fromList [("_lim", ["1"])]

defaultPL2 :: Params
defaultPL2 = M.fromList [("_lim", ["2"])]

emptyO :: Object
emptyO = HM.empty

emptyP :: Params
emptyP = M.empty

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

getResponseBody :: MonadIO m => Response -> m ByteString
getResponseBody = liftIO . ST.getResponseBody

insertLink :: ByteString -> Params -> Params
insertLink q = M.insert k [C8.drop 1 v]
    where
        (k, v) = C8.break (== '=') $ takeWhileEnd (/= '?') q

logReq :: MonadIO m => FilePath -> RequestBuilder IO () -> m ()
logReq tag req = liftIO $ do
    req' <- buildRequest req
    req'' <- requestToString req'
    logReq' tag req''

logReq' :: MonadIO m => FilePath -> ByteString -> m ()
logReq' tag req = liftIO $ do
    createDirectoryIfMissing True $ takeDirectory f
    let (header, body) = splitLog req
    writeFileBS (f <.> logExtH) header
    writeFileBS (f <.> logExtB) body
    where
        f = logDir </> tag

logRes :: MonadIO m => FilePath -> Response -> m ()
logRes tag res = liftIO $ do
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

putJSON :: (MonadIO m, ToJSON a) => ByteString -> a -> RequestBuilder m ()
putJSON u p = put u "application/json" $ toStrict $ encode p

readProcessText :: Text -> [Text] -> Text -> IO Text
readProcessText cmd args input = toText <$>
    readProcess (toString cmd) (toString <$> args) (toString input)

runRequest :: RequestBuilder IO () -> Hs.SnapHspecM b Response
runRequest req = do
  (Hs.SnapHspecState _ site app is _ bef aft) <- St.get
  res_ <- liftIO $ Hs.runHandlerSafe req (bef >> site >> aft) app is
  case res_ of
    Left err  -> error err
    Right res -> return res

shouldBe :: (Eq a, Show a) => a -> a -> Hs.SnapHspecM b ()
shouldBe a e = if a == e
    then setPass
    else setFail $ show a <> " /= " <> show e
infix 1 `shouldBe`

shouldContain :: Text -> Text -> Hs.SnapHspecM b ()
shouldContain s e = if e `T.isInfixOf` s
    then setPass
    else setFail $ show e <> " notMember " <> show s
infix 1 `shouldContain`

shouldMeasure :: (Foldable t, Show (t a)) => t a -> Int -> Hs.SnapHspecM b ()
shouldMeasure a l = if length a == l
    then setPass
    else setFail $ "|" <> show a <> "| /= " <> show l
infix 1 `shouldMeasure`

shouldNotBe :: (Eq a, Show a) => a -> a -> Hs.SnapHspecM b ()
shouldNotBe a e = if a /= e
    then setPass
    else setFail $ show a <> " == " <> show e
infix 1 `shouldNotBe`

shouldSatisfy :: Show a => a -> (a -> Bool) -> Hs.SnapHspecM b ()
shouldSatisfy a p = if p a
    then setPass
    else setFail $ show a <> " unsatisfied"
infix 1 `shouldSatisfy`


logDir :: FilePath
logDir = "test/log"

logExtH :: FilePath
logExtH = ".header"

logExtB :: FilePath
logExtB = ".body"

setFail :: Text -> Hs.SnapHspecM b ()
setFail = Hs.setResult . Hs.Failure Nothing . Hs.Reason . toString

setPass :: Hs.SnapHspecM b ()
setPass = Hs.setResult Hs.Success

splitLog :: ByteString -> (ByteString, ByteString)
splitLog log = (header, body')
    where
        sep = "\r\n\r\n"
        (header, body) = C8.breakSubstring sep log
        body' = fromMaybe "" $ C8.stripPrefix sep body

takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
takeWhileEnd p = C8.reverse . C8.takeWhile p . C8.reverse
