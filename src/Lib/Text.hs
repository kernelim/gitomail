module Lib.Text (
    showT
    , safeDecode
    , (+@)
    , removeTrailingNewLine
    , leadingZeros
    ) where

------------------------------------------------------------------------------------
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8With)
import           Data.Text.Encoding.Error (lenientDecode)
------------------------------------------------------------------------------------

removeTrailingNewLine :: Text -> Text
removeTrailingNewLine x =
    let n = T.length x - 1
    in if n >= 0  &&  T.index x n == '\n' then T.take n x else x

(+@) :: Text -> Text -> Text
(+@) a b = T.concat [a, b]

showT :: Show a => a -> Text
showT = T.pack . show

leadingZeros :: Int -> Text -> Text
leadingZeros n t = T.concat [ T.pack(take (n - (T.length t)) $ repeat '0'), t ]

safeDecode :: ByteString -> Text
safeDecode = decodeUtf8With lenientDecode

infixr 2 +@
