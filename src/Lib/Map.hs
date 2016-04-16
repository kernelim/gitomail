module Lib.Map (describeDifferences, MapDiff(..), newValues) where

import           Data.Map                   (Map)
import qualified Data.Map                   as Map

data MapDiff k v = MapDiff
    { inFirst          :: Map k v
    , inSecond         :: Map k v
    , inBothModified   :: Map k (v, v)
    , inBothUnmodified :: Map k v
    }

newValues :: Ord k => MapDiff k a -> Map k a
newValues md = (Map.map snd (inBothModified md)) `Map.union` (inSecond md)

describeDifferences :: (Eq v, Ord k) => Map k v -> Map k v -> MapDiff k v
describeDifferences a b = root
    where root = MapDiff x y z w
          x = Map.difference a b
          y = Map.difference b a
          f (s, t) = if s == t then Right s
                               else Left (s, t)
          i = Map.intersectionWith (\s t -> (s, t)) a b
          (z, w) = Map.mapEither f i

