module TiredPixel.Common.SQLite.Meta (
    migrate,
    ) where


import           Database.SQLite.Simple.Types
import           TiredPixel.Common.SQLite.Query
import qualified TiredPixel.Common.SQLite.Conn  as D


migrate :: [(Integer, D.Conn -> IO ())] -> D.Conn -> IO ()
migrate migrations d = do
    executeW' q0 d
    forM_ migrations (\(mId, mF) -> withTransaction d $ do
        migrationV <- rMigrationV d
        if mId > migrationV
            then mF d >> uMigrationV mId d
            else pass)
    where
        q0 = " \
        \   /* migrate.0 */ \
        \   CREATE TABLE IF NOT EXISTS _meta ( \
        \       k TEXT PRIMARY KEY, \
        \       v TEXT NOT NULL \
        \   ) \
        \ "


rMigrationV :: D.Conn -> IO Integer
rMigrationV d = do
    v_ <- listToMaybe <$> queryR' q d
    return $ fromOnly $ fromMaybe (Only vDef) v_
    where
        q = " \
        \   /* rMigrationV */ \
        \   SELECT CAST(v AS INTEGER) FROM _meta WHERE k = 'migration_v' \
        \ "
        vDef = 0

uMigrationV :: Integer -> D.Conn -> IO ()
uMigrationV mId = executeW q p
    where
        q = " \
        \   /* uMigrationV */ \
        \   INSERT INTO _meta VALUES ('migration_v', ?) \
        \       ON CONFLICT (k) DO UPDATE SET v = ? \
        \ "
        p = (mId, mId)
