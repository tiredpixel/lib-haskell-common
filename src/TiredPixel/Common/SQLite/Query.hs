module TiredPixel.Common.SQLite.Query (
    S.Only,
    executeManyW,
    executeW ,
    executeW',
    queryR ,
    queryR',
    queryW,
    setForeignKeys,
    withTransaction,
    ) where


import qualified Database.SQLite.Simple        as S
import qualified TiredPixel.Common.SQLite.Conn as D


executeManyW :: (MonadIO m, S.ToRow q) => S.Query -> [q] -> D.Conn -> m ()
executeManyW q p d = liftIO $ S.executeMany (D.db d) q p

executeW :: (MonadIO m, S.ToRow p) => S.Query -> p -> D.Conn -> m ()
executeW q p d = liftIO $ S.execute (D.db d) q p

executeW' :: MonadIO m => S.Query -> D.Conn -> m ()
executeW' q d = liftIO $ S.execute_ (D.db d) q

queryR :: (MonadIO m, S.ToRow p, S.FromRow r) => S.Query -> p -> D.Conn -> m [r]
queryR q p d = liftIO $ S.query (D.db d) q p

queryR' :: (MonadIO m, S.FromRow r) => S.Query -> D.Conn -> m [r]
queryR' q d = liftIO $ S.query_ (D.db d) q

queryW :: (MonadIO m, S.ToRow q, S.FromRow r) => S.Query -> q -> D.Conn -> m [r]
queryW q p d = liftIO $ S.query (D.db d) q p

setForeignKeys :: MonadIO m => Bool -> D.Conn -> m ()
setForeignKeys v = executeW' $ if v then q1 else q0
    where
        q0 = " \
        \   /* setForeignKeys.0 */ \
        \   PRAGMA foreign_keys = 0 \
        \ "
        q1 = " \
        \   /* setForeignKeys.1 */ \
        \   PRAGMA foreign_keys = 1 \
        \ "

withTransaction :: MonadIO m => D.Conn -> IO a -> m a
withTransaction d = liftIO . S.withTransaction (D.db d)
