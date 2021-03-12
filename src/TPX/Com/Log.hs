{-# LANGUAGE RankNTypes #-}


module TPX.Com.Log (
    L.Logger,
    withLog,
    trace,
    debug,
    info,
    warn,
    err,
    fatal,
    ) where


import           Control.Exception     (bracket)
import           Prelude               hiding (trace)
import qualified System.Logger         as L
import qualified System.Logger.Message as M


withLog :: forall c. (L.Logger -> IO c) -> IO c
withLog = bracket (L.new L.defSettings) L.close

trace :: (MonadIO m) => L.Logger -> Text -> m ()
trace l = L.trace l . M.msg

debug :: (MonadIO m) => L.Logger -> Text -> m ()
debug l = L.debug l . M.msg

info :: (MonadIO m) => L.Logger -> Text -> m ()
info l = L.info l . M.msg

warn :: (MonadIO m) => L.Logger -> Text -> m ()
warn l = L.warn l . M.msg

err :: (MonadIO m) => L.Logger -> Text -> m ()
err l = L.err l . M.msg

fatal :: (MonadIO m) => L.Logger -> Text -> m ()
fatal l = L.fatal l . M.msg
