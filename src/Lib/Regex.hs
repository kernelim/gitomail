{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Lib.Regex (matchWhole) where

------------------------------------------------------------------------------------
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Regex.TDFA             ((=~))
import           Text.Regex.TDFA.Text        ()
import           Text.Regex.Base.RegexLike   (RegexLike)
import           Text.Regex.TDFA.Common      (Regex)
------------------------------------------------------------------------------------

matchWhole :: RegexLike Regex a => Text -> a -> Bool
matchWhole regex ref = (ref =~ (T.concat ["^", regex, "$"])) :: Bool
