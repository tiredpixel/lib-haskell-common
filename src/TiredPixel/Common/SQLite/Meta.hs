module TiredPixel.Common.SQLite.Meta (
    migrate,
    ) where


import           Database.SQLite.Simple.Types
import qualified TiredPixel.Common.SQLite.Conn  as D
import           TiredPixel.Common.SQLite.Query


migrate :: [(Integer, D.Conn -> IO ())] -> Text -> D.Conn -> IO ()
migrate migrations schema d = do
    executeW' q0 d
    forM_ migrations (\(mId, mF) -> withTransaction d $ do
        migrationV <- rMigrationV kMigrationV d
        if mId > migrationV
            then mF d >> uMigrationV mId kMigrationV d
            else pass)
    where
        kMigrationV = schema <> ".migration_v"
        q0 = " \
        \   /* migrate.0 */ \
        \   CREATE TABLE IF NOT EXISTS _meta ( \
        \       k TEXT PRIMARY KEY, \
        \       v TEXT NOT NULL \
        \   ) \
        \ "


rMigrationV :: Text -> D.Conn -> IO Integer
rMigrationV k d = do
    v_ <- listToMaybe <$> queryR q p d
    return $ fromOnly $ fromMaybe (Only vDef) v_
    where
        vDef = 0
        q = " \
        \   /* rMigrationV */ \
        \   SELECT CAST(v AS INTEGER) FROM _meta WHERE k = ? \
        \ "
        p = [k]

uMigrationV :: Integer -> Text -> D.Conn -> IO ()
uMigrationV mId k = executeW q p
    where
        q = " \
        \   /* uMigrationV */ \
        \   INSERT INTO _meta VALUES (?, ?) \
        \       ON CONFLICT (k) DO UPDATE SET v = ? \
        \ "
        p = (k, mId, mId)
