module Lib.Memo (cacheIO, cacheIO') where

------------------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.State    (MonadIO)
import           Data.IORef
------------------------------------------------------------------------------------

-- Roughly based on some code from https://github.com/lamdu/lamdu
cacheIO :: (MonadIO m, Eq a) => (a -> m b) -> m (a -> m b)
cacheIO f =
    do  ref <- liftIO $ newIORef Nothing
        return $ \x -> do
            r <- liftIO $ readIORef ref
            let
                actual_io = do
                    l <- f x
                    liftIO $ writeIORef ref $ Just (l, x)
                    return l
            case r of
                Nothing -> actual_io
                Just (l, orig_x)
                    | orig_x == x -> return l
                    | otherwise -> actual_io

cacheIO' :: (MonadIO m) => m b -> m (m b)
cacheIO' f = fmap (\x -> x ()) (cacheIO $ \() -> f)
