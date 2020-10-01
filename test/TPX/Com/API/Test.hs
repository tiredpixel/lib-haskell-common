module TPX.Com.API.Test (
    module E,
    assertElemN,
    postJSON,
    shouldContainSubseq
    ) where


import              Control.Lens                            as  E
import              Data.Aeson                              as  E   hiding  ((.=))
import              Data.Aeson.Lens                         as  E
import              Data.List                               (isSubsequenceOf)
import              Snap.Core                               as  E   hiding  (addHeader, setContentType, setHeader)
import              Snap.Test                               as  E
import              TPX.Com.API.Aeson                       as  E
import              Test.Hspec                              as  E


assertElemN :: Response -> Int -> IO ()
assertElemN res n = do
    b <- getResponseBody res
    length (b ^. _Object) `shouldBe` n

postJSON :: (MonadIO m, ToJSON a) => ByteString -> a -> RequestBuilder m ()
postJSON u p = postRaw u "application/json" $ toStrict $ encode p

shouldContainSubseq :: Eq a => [a] -> [a] -> Expectation
shouldContainSubseq a b = (b `isSubsequenceOf` a) `shouldBe` True
