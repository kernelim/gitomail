module Lib.Sequence
    ( seqConcat
    , unfoldrES
    , unfoldrS
    ) where

------------------------------------------------------------------------------------
import           Data.Sequence                 (Seq, empty, (><))
------------------------------------------------------------------------------------

seqConcat :: Seq (Seq a) -> Seq a
seqConcat s = foldl (><) empty s

unfoldrES :: (b -> Either String (Maybe (b, Seq a))) -> b -> (Either String (Seq a), b)
unfoldrES f = unfoldr' empty
    where unfoldr' as b = either (\s -> (Left s, b)) (z as b)  $ f b
          z as b        = maybe (Right as, b)  $ \(b', a) -> unfoldr' (as >< a) b'

unfoldrS :: (b -> Maybe (b, Seq a)) -> b -> (Seq a, b)
unfoldrS f = unfoldr' empty
    where unfoldr' as b = maybe (as, b) (\(b', a) -> unfoldr' (as >< a) b') $ f b
