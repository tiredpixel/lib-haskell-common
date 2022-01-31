module TiredPixel.Common.Logger (
    module Control.Monad.Logger,
    runLogger,
    ) where


import Control.Monad.Logger


runLogger :: MonadIO m => LoggingT m a -> m a
runLogger = runStdoutLoggingT . filterLogger p
    where
        p _ lvl = lvl > LevelDebug
