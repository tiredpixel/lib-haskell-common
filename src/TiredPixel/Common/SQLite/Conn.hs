module TiredPixel.Common.SQLite.Conn (
    Conn(..),
    closeConn,
    openConnS,
    ping,
    withConnS,
    ) where


import           Control.Exception      (bracket)
import qualified Database.SQLite.Simple as S


newtype Conn = ConnS { db :: S.Connection }

closeConn :: Conn -> IO ()
closeConn (ConnS d) = S.close d

openConnS :: IO Conn
openConnS = do
    f <- fromMaybe fDef <$> lookupEnv "SQLITE_FILE"
    d <- S.open f
    return ConnS {
        db = d}
    where
        fDef = "sqlite3.db"

ping :: MonadIO m => Conn -> m (Maybe (S.Only Text))
ping d = liftIO $ listToMaybe <$> S.query_ (db d) q
    where
        q = " \
        \   /* ping */ \
        \   SELECT sqlite_version() \
        \ "

withConnS :: (Conn -> IO c) -> IO c
withConnS = bracket openConnS closeConn
