{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

module Lib.LiftedIO
       (print, putStrLn, newIORef, modifyIORef', readIORef,
        writeIORef, IOREF.IORef)
       where

------------------------------------------------------------------------------------
import qualified Prelude as P
import           Control.Monad.State            (MonadIO)
import           Control.Monad.IO.Class         (liftIO)
import           Prelude ((.), Show, ($))
import qualified Data.IORef as IOREF
------------------------------------------------------------------------------------

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . P.print

putStrLn :: MonadIO m => P.String -> m ()
putStrLn = liftIO . P.putStrLn

newIORef :: MonadIO m => a -> m (IOREF.IORef a)
newIORef = liftIO. IOREF.newIORef

modifyIORef' :: MonadIO m => IOREF.IORef a -> (a -> a) -> m ()
modifyIORef' x y = liftIO $ IOREF.modifyIORef' x y

readIORef :: MonadIO m => IOREF.IORef a -> m a
readIORef = liftIO . IOREF.readIORef

writeIORef :: MonadIO m => IOREF.IORef a -> a ->  m ()
writeIORef x !y = liftIO $ IOREF.writeIORef x y
