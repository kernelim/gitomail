module Lib.Monad (
    mapWithKeyM,
    foldSubJoinT21toT12M,
    mapMM_,
    whenM,
    whenM') where

------------------------------------------------------------------------------------
import           Control.Monad                  (foldM, forM, when, void)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
------------------------------------------------------------------------------------

mapWithKeyM :: (Ord k, Functor m, Monad m) => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM f m = fmap Map.fromList $
  forM (Map.toList m) $ \(k, a) -> do
    i <- f k a
    return (k, i)

foldSubJoinT21toT12M :: Monad m => [(t1, [(t2, t)])] -> a -> (a -> ((t1, t2), t) -> m a) -> m a
foldSubJoinT21toT12M lst s f' = foldM l2 s lst
  where
    l2 s2 (t1, t2t) = foldM l3 s2 t2t
      where
        l3 s3 (t2, t) = f' s3 ((t1, t2), t)

mapMM_ :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
mapMM_ a b = a >>= (mapM_ b)

whenM :: Monad m => m Bool -> m () -> m ()
whenM a b = do
    v <- a
    when v b

whenM' :: Monad m => m Bool -> m a -> m ()
whenM' a b = do
    v <- a
    when v (void b)
