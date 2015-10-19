module Lib.Text (showT, (+@), removeTrailingNewLine) where

------------------------------------------------------------------------------------
import           Data.Text (Text)
import qualified Data.Text as T
------------------------------------------------------------------------------------

removeTrailingNewLine :: Text -> Text
removeTrailingNewLine x =
    let n = T.length x - 1
    in if n >= 0  &&  T.index x n == '\n' then T.take n x else x

(+@) :: Text -> Text -> Text
(+@) a b = T.concat [a, b]

showT :: Show a => a -> Text
showT = T.pack . show

infixr 2 +@
