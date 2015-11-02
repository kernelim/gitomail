{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib.Monad
    ( mapWithKeyM
    , foldSubJoinT21toT12M
    , mapMM_
    , whenM
    , whenM'
    , seqMapM
    , seqForM
    , lSeqMapM
    , lSeqForM
    , dlistMapM
    , dlistForM
    ) where

------------------------------------------------------------------------------------
import           Control.Monad                  (foldM, when, void)
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Sequence                  (Seq)
import qualified Data.Sequence                  as Seq
import           Data.DList                     (DList)
import qualified Data.DList                     as DList
import           Data.Foldable                 (toList)
------------------------------------------------------------------------------------

mapWithKeyM :: (Ord k, Functor m, Monad m) => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM f m = fmap Map.fromList $
   lSeqForM (Map.toList m) $ \(k, a) -> do
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

--
-- Left monadic fold of a Foldable into a Sequence.
--
-- This is safer in a sense that it won't cause stack overflow, compared
-- to the standard mapM. E.g. seqMapM return [1..10000000 :: Int] >> return ()
--
seqMapM :: (Monad m, Foldable l) =>
           (a -> m b) -> l a -> m (Seq b)
seqMapM f = foldM item Seq.empty
    where item acc x = f x >>= return . (acc Seq.|>)

dlistMapM :: (Monad m, Foldable l) =>
           (a -> m b) -> l a -> m (DList b)
dlistMapM f = foldM item DList.empty
    where item acc x = f x >>= return . (acc `DList.snoc`)

lSeqMapM :: (Monad m, Foldable l) =>
            (a -> m b) -> l a -> m [b]
lSeqMapM m l = fmap toList $ seqMapM m l

seqForM :: (Monad m, Foldable l) =>
           l a -> (a -> m b) -> m  (Seq b)
seqForM = flip seqMapM

lSeqForM :: (Monad m, Foldable l) =>
            l a -> (a -> m b) -> m [b]
lSeqForM l m = fmap toList $ seqForM l m

dlistForM :: (Monad m, Foldable l) =>
           l a -> (a -> m b) -> m  (DList b)
dlistForM = flip dlistMapM
