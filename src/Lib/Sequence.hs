module Lib.Sequence (
    seqConcat
    ) where

------------------------------------------------------------------------------------
import           Data.Sequence                 (Seq, empty, (><))
------------------------------------------------------------------------------------

seqConcat :: Seq (Seq a) -> Seq a
seqConcat s = foldl (><) empty s

