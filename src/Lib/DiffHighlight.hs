{-# LANGUAGE OverloadedStrings #-}

module Lib.DiffHighlight (
    highlight,
    parseDiff,
    ParsedDiff,
    DiffHeader,
    DiffContent,
    OtherContent
    ) where

------------------------------------------------------------------------------------
import           Data.Foldable                 (toList)
import           Data.Text                     (Text)
import qualified Data.Char                     as C
import qualified Data.Text                     as T
import qualified Data.Algorithm.Patience       as DP
import qualified Data.List                     as DL
import qualified Data.DList                    as DList
----
import           Lib.Text                      (lineSplit, removeTrailingNewLine)
import           Lib.DList                     (dlistConcat)
import           Lib.Formatting                (FList, Fragment(..), Format(..),
                                               mkFormS, fragmentize, flistToText)
------------------------------------------------------------------------------------

type OtherContent = [(Text, Format)]
type DiffHeader = [(Text, Format)]
type DiffContent = [(Text, Format)]

type ParsedDiff = [Either OtherContent (DiffHeader, DiffContent)]

parseDiff :: Text -> ParsedDiff
parseDiff text = fileGroups
    where
        parsed          = parse $ lineSplit text

        parse (x:xs) = case' "diff "          (const DiffMain      ) diff x xs $
                       else' parse x xs
        parse []     = []

        fn = removeTrailingNewLine . (T.drop 6)

        diff (x:xs) = case' "index "         (const DiffMainExtra) diff x xs $
                      case' "new "           (const DiffMainExtra) diff x xs $
                      case' "old "           (const DiffMainExtra) diff x xs $
                      case' "delete "        (const DiffMainExtra) diff x xs $
                      case' "copy "          (const DiffMainExtra) diff x xs $
                      case' "rename "        (const DiffMainExtra) diff x xs $
                      case' "similarity "    (const DiffMainExtra) diff x xs $
                      case' "dissimilarity " (const DiffMainExtra) diff x xs $
                      case' "--- "           (DiffRemoveFile . fn) diff x xs $
                      case' "+++ "           (DiffAddFile    . fn) hunk x xs $
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
               then (x, (mark x)):next xs
               else alt

        else' f x xs = (x, (DiffUnchanged)):(f xs)

        fileGroups      = iterFiles $ DL.groupBy g parsed
            where
                isDiffStart (DiffMain        ) = True
                isDiffStart (DiffMainExtra   ) = True
                isDiffStart (DiffAddFile    _) = True
                isDiffStart (DiffRemoveFile _) = True
                isDiffStart _                  = False

                g (_, a) (_, b) = isDiffStart a == isDiffStart b
                iterFiles (diffMeta@((_, DiffMain):_):content:xxs) =
                     Right (diffMeta, content):(iterFiles xxs)
                iterFiles (x:xxs) = (Left x):(iterFiles xxs)
                iterFiles [] = []

highlight :: Text -> FList
highlight = onlyHighlight . parseDiff

onlyHighlight :: ParsedDiff -> FList
onlyHighlight parsedDiff = mkFormS MonospacePar $ diffHighlight
    where
        parsedDiffList  = concat $ map deEither parsedDiff
            where
                deEither (Left lst)     = lst
                deEither (Right (r, l)) = r ++ l

        diffHighlight   = intraLineDiff $ fragmentize' $ parsedDiffList
        fragmentize'    = fragmentize . (map (\(x, y) ->(x, Just y)))

        intraLineDiff :: (Foldable r) => r Fragment -> FList
        intraLineDiff = root'
            where
                root' x = DList.fromList $ iterLines $ toList x
                iterLines []                                            = []
                iterLines ((TForm DiffRemove rs):(TForm DiffAdd as):xs) =
                      let (rs', as') = mkDiff rs as
                       in (TForm DiffRemove rs'):(TForm DiffAdd as'):(iterLines xs)
                iterLines (x:xs)                                        = x:(iterLines xs)
                unmarkTrailingWhitespace = remarkInBetween . crux
                    where
                        crux (a@(_, Just Mark):b@(_, Nothing):c@(_, Just Mark):xs) =
                            a:b:crux (c:xs)
                        crux ((t', Just Mark):xs) =
                            case spanAround C.isSpace t' of
                                ("", t, "") -> (t, Just Mark) :crux xs
                                (a , t, "") -> (a, Nothing)   :(t, Just Mark):crux xs
                                ("", t,  b) -> (t, Just Mark) :(b, Nothing)     :crux xs
                                (a , t,  b) -> (a, Nothing)   :(t, Just Mark):(b, Nothing):crux xs
                        crux (z:xs) = z:crux xs
                        crux []            = []
                        rspan f t = let (x, y) = T.span f $ T.reverse t
                                     in (T.reverse x, T.reverse y)
                        spanAround f t = let (s, a) = T.span f t
                                             (e, b) = rspan f a
                                          in (s, b, e)
                        fc x = C.isSpace x

                        remarkInBetween (a@(at, Just Mark):xs) =
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
                            (T.concat $ map fst $ reverse lst, Just Mark):(remarkInBetween $ b:xs)
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
                        rtDiffed               = dlistConcat $ DList.fromList $ map fst zDiffed
                        dtDiffed               = dlistConcat $ DList.fromList $ map snd zDiffed
                        fDiff (a, b)           =
                            let c              = DP.diff (tokenize $ T.drop 1 a)
                                                         (tokenize $ T.drop 1 b)
                             in (fragmentize $ unmarkTrailingWhitespace $ ("-", Nothing):old c,
                                 fragmentize $ unmarkTrailingWhitespace $ ("+", Nothing):new c)
                        old ((DP.Old t   ):xs) = (t, Just Mark):(old xs)
                        old ((DP.Both t _):xs) = (t, Nothing     ):(old xs)
                        old ((DP.New _   ):xs) = old xs
                        old []                 = []
                        new ((DP.New t   ):xs) = (t, Just Mark):(new xs)
                        new ((DP.Both t _):xs) = (t, Nothing     ):(new xs)
                        new ((DP.Old _   ):xs) = new xs
                        new []                 = []
                        diffed = if length rtLines /= length dtLines
                                    then (rt, dt)
                                    else (rtDiffed, dtDiffed)
                     in diffed
