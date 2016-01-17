{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Text (
    showT
    , safeDecode
    , (+@)
    , removeTrailingNewLine
    , leadingZeros
    , lineSplit
    , lineSplitAfter
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

-- | A line split function that preserves all character, unlike
-- the standard 'lines' function, where `lines "foo\n" == lines "foo"`.
lineSplit :: Text -> [Text]
lineSplit t = reverse $ r $ reverse $ map f z
    where s            = T.splitOn ("\n") t
          n            = length s
          z            = zip s (map (== n) [1..])
          f (x, False) = T.concat [x, "\n"]
          f (x, True)  = x
          r ("":xs)    = xs
          r xs         = xs

-- | Another line split that preserves all characters, but this version
-- | always puts the break after the newline.
lineSplitAfter :: Text -> [Text]
lineSplitAfter t = z
     where s = T.splitOn ("\n") t
           n = length s
           z = map (\(x, y) -> x +@ y) $ zip s (map (\x -> if x == n then "" else "\n") [1..n])
