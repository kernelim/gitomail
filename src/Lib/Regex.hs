{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib.Regex
    ( matchWhole
    , splitByCaptures
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
                                              match, MatchArray)
import qualified Data.Array                  as Array
import           Data.Array.Unboxed          (listArray, UArray, (!))
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

-- splitByCaptures :: Array Int MatchArray
-- (MatchOffset,MatchLength)]

type AText = UArray Int Char

textToAText :: Text -> AText
textToAText t = root
    where n = T.length t
          root = listArray (0, n - 1) (T.unpack t)

subAText :: AText -> Int -> Int -> Text
subAText t a len = T.pack (map (t !) [a .. a + len - 1])

splitByCaptures :: RegexE a => a -> Int ->  Text -> [Either Text Text]
splitByCaptures regexStr captureIdx inp =
    let inpLength = T.length inp
        captures = map (\a -> a Array.! captureIdx) (inp =~+ regexStr :: [MatchArray])
        captures' = [Nothing] ++ map Just captures ++ [Nothing]
        joins = zip captures' (drop 1 captures')
        inpArray = textToAText inp
        sub = subAText inpArray
        makePart (Nothing, Just (idx, _)) =
            [Left $ sub 0 idx]
        makePart (Just (idx, size), Nothing) =
            [Right $ sub idx size, Left $ sub (idx + size) (inpLength - (idx + size))]
        makePart (Just (idx1, size1), Just (idx2, _)) =
            [Right $ sub idx1 size1, Left $ sub (idx1 + size1) (idx2 - (idx1 + size1))]
        makePart (Nothing, Nothing) = []
        parts = case concat $ map makePart joins of
                   [] -> [Left inp]
                   r -> r
     in parts

matchWhole :: RegexLike Regex a => Text -> a -> Bool
matchWhole regex ref = (ref =~+ (T.concat ["^", regex, "$"])) :: Bool
