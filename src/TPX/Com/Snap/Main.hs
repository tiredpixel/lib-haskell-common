module TPX.Com.Snap.Main (
    config,
    init,
    wait,
    ) where


import Control.Concurrent      (ThreadId, killThread)
import Prelude                 hiding (init)
import Snap.Core
import Snap.Http.Server.Config
import System.IO
import System.Posix.Signals


config :: MonadSnap m => Config m a
config =
    setAccessLog (ConfigFileLog "-") $
    setErrorLog (ConfigFileLog "-")
    defaultConfig

init :: IO (MVar (Either Text Text))
init = newEmptyMVar

wait :: MVar (Either Text Text) -> ThreadId -> IO ()
wait done tId = do
    _ <- installHandler sigTERM sigTERMH Nothing
    doneM <- takeMVar done
    case doneM of
        Left  msg -> hPutStrLn stderr (toString msg) >> exitFailure
        Right msg -> hPutStrLn stderr (toString msg)
    hPutStrLn stderr "Čau"
    where
        sigTERMH = CatchOnce $ do
            hPutStrLn stderr "Handling SIGTERM"
            killThread tId
            putMVar done $ Right "Terminated"
