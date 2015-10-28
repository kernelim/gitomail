{-# LANGUAGE OverloadedStrings #-}

module Lib.DiffHighlight (highlight) where

------------------------------------------------------------------------------------
import           Data.Foldable                 (toList)
import qualified Data.Sequence                 as Seq
import           Data.Text                     (Text)
import qualified Data.Char                     as C
import qualified Data.Text                     as T
import qualified Data.Algorithm.Patience       as DP
----
import           Lib.Text                      (lineSplit)
import           Lib.Sequence                  (seqConcat)
import           Lib.InlineFormatting          (FList, Fragment(..), Format(..),
                                               mkFormS, fragmentize, flistToText)
------------------------------------------------------------------------------------

highlight :: Text -> FList
highlight text = mkFormS MonospacePar root
    where
        root         = intraLineDiff $ fragmentize $ parse $ lineSplit text
        parse (x:xs) = case' "diff "          (const DiffMain      ) diff x xs $
                       else' parse x xs
        parse []     = []

        diff (x:xs) = case' "index "         (const DiffMainExtra ) diff x xs $
                      case' "new "           (const DiffMainExtra ) diff x xs $
                      case' "old "           (const DiffMainExtra ) diff x xs $
                      case' "delete "        (const DiffMainExtra ) diff x xs $
                      case' "copy "          (const DiffMainExtra ) diff x xs $
                      case' "rename "        (const DiffMainExtra ) diff x xs $
                      case' "similarity "    (const DiffMainExtra ) diff x xs $
                      case' "dissimilarity " (const DiffMainExtra ) diff x xs $
                      case' "--- "           (DiffRemoveFile      ) diff x xs $
                      case' "+++ "           (DiffAddFile         ) hunk x xs $
                      else'                  diff x xs
        diff []     = []

        hunk (x:xs) = case' "@@ "    (const DiffHunkHeader) hunk x xs $
                      case' "-"      (const DiffRemove    ) hunk x xs $
                      case' "+"      (const DiffAdd       ) hunk x xs $
                      case' "diff "  (const DiffMain      ) diff x xs $
                      else' hunk x xs
        hunk []     = []

        -- Infra
        case' pref mark next x xs alt =
            if pref `T.isPrefixOf` x
               then (x, (Just (mark x))):next xs
               else alt

        else' f x xs = (x, (Just DiffUnchanged)):(f xs)

        intraLineDiff = root'
            where
                root' x = Seq.fromList $ iterLines $ toList x
                iterLines []                                            = []
                iterLines ((TForm DiffRemove rs):(TForm DiffAdd as):xs) =
                      let (rs', as') = mkDiff rs as
                       in (TForm DiffRemove rs'):(TForm DiffAdd as'):(iterLines xs)
                iterLines (x:xs)                                        = x:(iterLines xs)
                unmarkTrailingWhitespace = remarkInBetween . crux
                    where
                        crux (a@(_, Just Inverse):b@(_, Nothing):c@(_, Just Inverse):xs) =
                            a:b:crux (c:xs)
                        crux ((t', Just Inverse):xs) =
                            case spanAround C.isSpace t' of
                                ("", t, "") -> (t, Just Inverse) :crux xs
                                (a , t, "") -> (a, Nothing)      :(t, Just Inverse):crux xs
                                ("", t,  b) -> (t, Just Inverse) :(b, Nothing)     :crux xs
                                (a , t,  b) -> (a, Nothing)      :(t, Just Inverse):(b, Nothing):crux xs
                        crux (z:xs) = z:crux xs
                        crux []            = []
                        rspan f t = let (x, y) = T.span f $ T.reverse t
                                     in (T.reverse x, T.reverse y)
                        spanAround f t = let (s, a) = T.span f t
                                             (e, b) = rspan f a
                                          in (s, b, e)
                        fc x = C.isSpace x

                        remarkInBetween (a@(at, Just Inverse):xs) =
                            if T.dropAround fc at /= ""
                                then seekNextNonwhitespace [a] xs
                                else a:remarkInBetween xs
                        remarkInBetween (z:xs) = z:remarkInBetween xs
                        remarkInBetween []     = []

                        seekNextNonwhitespace lst (b@(at, Nothing):xs) =
                            if T.dropAround fc at == ""
                               then seekNextNonwhitespace (b:lst) xs
                               else (reverse lst) ++ (remarkInBetween (b:xs))
                        seekNextNonwhitespace lst (b@(_, Just _):xs) =
                            (T.concat $ map fst $ reverse lst, Just Inverse):(remarkInBetween $ b:xs)
                        seekNextNonwhitespace lst []     = reverse lst

                tokenize t = T.groupBy sep t
                sep a b
                    | C.isAlphaNum a && C.isAlphaNum b = True
                    | otherwise                        = False
                mkDiff rt dt =
                    let rtLines                = lineSplit $ flistToText rt
                        dtLines                = lineSplit $ flistToText dt
                        zLines                 = zip rtLines dtLines
                        zDiffed                = map fDiff zLines
                        rtDiffed               = seqConcat $ Seq.fromList $ map fst zDiffed
                        dtDiffed               = seqConcat $ Seq.fromList $ map snd zDiffed
                        fDiff (a, b)           =
                            let c              = DP.diff (tokenize $ T.drop 1 a)
                                                         (tokenize $ T.drop 1 b)
                             in (fragmentize $ unmarkTrailingWhitespace $ ("-", Nothing):old c,
                                 fragmentize $ unmarkTrailingWhitespace $ ("+", Nothing):new c)
                        old ((DP.Old t   ):xs) = (t, Just Inverse):(old xs)
                        old ((DP.Both t _):xs) = (t, Nothing     ):(old xs)
                        old ((DP.New _   ):xs) = old xs
                        old []                 = []
                        new ((DP.New t   ):xs) = (t, Just Inverse):(new xs)
                        new ((DP.Both t _):xs) = (t, Nothing     ):(new xs)
                        new ((DP.Old _   ):xs) = new xs
                        new []                 = []
                        diffed = if length rtLines /= length dtLines
                                    then (rt, dt)
                                    else (rtDiffed, dtDiffed)
                     in diffed
