{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib.Regex
    ( matchWhole
    , (=~+)
    , (=~*)
    ) where

------------------------------------------------------------------------------------
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Text.Regex.TDFA.Text        ()
import           Text.Regex.TDFA.Common      (Regex, CompOption(..), ExecOption(..))
import           Text.Regex.Base.RegexLike   (RegexLike)
import           Text.Regex.Base             (RegexMaker, RegexContext, makeRegexOpts,
                                              match)
------------------------------------------------------------------------------------

type RegexE a = RegexMaker Regex CompOption ExecOption a

compile' :: RegexE a => a -> Regex
compile' r = let make :: RegexE a => a -> Regex
                 make = makeRegexOpts compOpts execOpts
                 execOpts = ExecOption {captureGroups = True}
                 compOpts = CompOption {caseSensitive = True,
                                        multiline = False,
                                        rightAssoc = True,
                                        newSyntax = True,
                                        lastStarGreedy = False}
               in make r

(=~+) :: (RegexE a, RegexContext Regex source target) =>
         source -> a -> target
t =~+ r = (match . compile') r t

(=~*) :: RegexContext regex source target =>
         source -> regex -> target
t =~* r = match r t

--

matchWhole :: RegexLike Regex a => Text -> a -> Bool
matchWhole regex ref = (ref =~+ (T.concat ["^", regex, "$"])) :: Bool
