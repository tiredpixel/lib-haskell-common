module TPX.Com.Cursor (
    Cursor(..),
    curse,
    ) where


data Cursor = Cursor {
    cursorPos :: Maybe (Either ByteString ByteString),
    cursorLim :: Integer
    } deriving (Show)

curse :: MonadIO m =>
    (Integer -> c -> m [a]) ->
    (Integer -> ByteString -> c -> m [a]) ->
    (Integer -> ByteString -> c -> m [a]) ->
    Cursor -> c -> m [a]
curse frst next prev cur d = case cursorPos cur of
    Nothing           -> frst lim d
    Just (Right posN) -> next lim posN d
    Just (Left  posP) -> prev lim posP d
    where
        lim = cursorLim cur
