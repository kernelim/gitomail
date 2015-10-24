{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Lib.InlineFormatting
       (ansiToFList, highlightDiff, combineFLists, FList,
        flistToInlineStyleHtml, flistToANSI, test, Format(..),
        flistToText, highlightText, highlightMonospace) where

------------------------------------------------------------------------------------
import qualified Data.ByteString               as BS
import           Data.ByteString.Search        (indices)
import           Data.Foldable                 (toList)
import qualified Data.Sequence                 as Seq
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Encoding            as T
import           Text.Blaze.Html               (toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
------------------------------------------------------------------------------------
import           Lib.Text                      ((+@), showT)
------------------------------------------------------------------------------------

data Format
    = DiffMain
    | DiffMainExtra
    | DiffHunkHeader
    | DiffAdd
    | DiffRemove
    | DiffAddFile
    | DiffRemoveFile
    | DiffUnchanged
    | Inverse
    | Emphesis Int
    | MonospacePar
    | Monospace
    | Underline
    | List
    | ListItem
    | Table
    | TableRow Int
    | TableCol Int Int
    | TableCellPad Int
    | Link Text
    | Footer
    | Dark
      deriving (Show, Eq, Ord)

type Fragment = (Text, [Format])
type FList = [Fragment]

ansiToFList :: Text -> Either String FList
ansiToFList t = do
    let te = T.encodeUtf8 t
        bStart = "\x1b[7m"
        bEnd   = "\x1b[27m"
        l = indices bStart te
        r = indices bEnd te
    z <- if length l /= length r
         then Left "unclosed inversing ANSI"
         else Right $ zip l r
    let
        in_between =
            [(0, head l)]
               ++ zip (map (+BS.length bEnd) $ init r) (tail l)
               ++ [(last r + BS.length bEnd, BS.length te)]

        rjoin a b = join a b

        join (a:as) (b:bs) = b:a:(join as bs)
        join (a:as) []     = a:as
        join [] (b:bs)     = b:bs
        join [] []         = []

        assign lst rval    = map (\x -> (x, rval)) lst

        z_cap          = map (\(a, b) -> (a + BS.length bStart, b)) z

        z_set          = assign z_cap      $ [Inverse]
        in_between_set = assign in_between $ []

        m ((s, e), rval) = (T.decodeUtf8 $ BS.take (e - s) $ BS.drop s te, rval)
        flists = if l == [] || r == []
                     then assign [t] []
                     else map m $ rjoin z_set in_between_set

    return flists

lineSplit :: Text -> [Text]
lineSplit t = map (\x->T.concat[x,"\n"]) $ T.splitOn ("\n") t

highlightMonospace :: Text -> FList
highlightMonospace t = [(t, [MonospacePar])]

highlightDiff :: Text -> FList
highlightDiff text = parse
    where
        parse       = root $ lineSplit text
        root (x:xs) = case' "diff "          DiffMain       diff x xs $
                      else' root x xs
        root []     = []

        diff (x:xs) = case' "index "         DiffMainExtra  diff x xs $
                      case' "new "           DiffMainExtra  diff x xs $
                      case' "old "           DiffMainExtra  diff x xs $
                      case' "delete "        DiffMainExtra  diff x xs $
                      case' "copy "          DiffMainExtra  diff x xs $
                      case' "rename "        DiffMainExtra  diff x xs $
                      case' "similarity "    DiffMainExtra  diff x xs $
                      case' "dissimilarity " DiffMainExtra  diff x xs $
                      case' "--- "           DiffRemoveFile diff x xs $
                      case' "+++ "           DiffAddFile    hunk x xs $
                      else'                  diff x xs
        diff []     = []

        hunk (x:xs) = case' "@@ "    DiffHunkHeader hunk x xs $
                      case' "-"      DiffRemove     hunk x xs $
                      case' "+"      DiffAdd        hunk x xs $
                      case' "diff "  DiffMain       diff x xs $
                      else' hunk x xs
        hunk []     = []

        -- Infra
        case' pref mark next x xs alt =
            if pref `T.isPrefixOf` x
               then (x, [MonospacePar, mark]):next xs
               else alt

        else' f x xs = (x, [MonospacePar, DiffUnchanged]):(f xs)

highlightText :: Text -> FList
highlightText text = map (\t -> (t, [])) $ lineSplit text

combineFLists :: FList -> FList -> Either String FList
combineFLists ((tx, fx):xs) ((ty, fy):ys) = do
        (c, nx, ny) <-
            case (T.stripPrefix tx ty, T.stripPrefix ty tx) of
                (Just "", Just "") -> Right ((tx, u), xs,         ys        )
                (Just r,  Nothing) -> Right ((tx, u), xs,         (r, fy):ys)
                (Nothing, Just r)  -> Right ((ty, u), (r, fx):xs, ys        )
                _ -> Left $ "Texts are not equal: " ++ (show (((tx, fx):xs), ((ty, fy):ys)))
        rest <- combineFLists nx ny
        return (c:rest)
    where
        u = fx ++ fy
combineFLists (x:xs) [] = Right (x:xs)
combineFLists [] (x:xs) = Right (x:xs)
combineFLists [] []    = Right []

data FormatPos = Start | End
    deriving Eq

flistToInlineStyleHtml :: Maybe (Bool -> Text -> Text) -> FList -> Text
flistToInlineStyleHtml fileURL l = crux
    where crux   = T.concat $ toList $ Seq.zipWith3 r seq'a seq' seq'b
          seq'   = Seq.fromList l
          seq'a  = ("", [])            Seq.<|  (Seq.take (Seq.length seq' - 1) seq')
          seq'b  = (Seq.drop 1 seq')   Seq.|>  ("", [])
          delink x               = T.replace "://" ":/&#8203;/" $ T.replace "." "&#8203;." x
          plain t                = T.concat $ map delink $ TL.toChunks $ renderHtml $ toHtml t
          substract la lb        = filter (not . (`elem` lb)) la
          r (_, a) (t, m) (_, b) = T.concat $ concat [map (html Start (t, m)) (am),
                                                      [plain t],
                                                      map (html End (t, m)) (reverse rm)]
                                   where am = m `substract` a  -- added to M
                                         rm = m `substract` b  -- remove from M
          linkStart h = T.concat ["<a href=\"" , h, "\" style=\"text-decoration: none\">"]
          diffStartFile n t color = T.concat [maybe "" (\f -> linkStart (f n (T.drop 6 t))) fileURL,
                                            "<div style=\"background: ", color, "; font-family: monospace\">" ]
          diffEndFile             = T.concat ["</div>", maybe "" (const "</a>") fileURL]

          html Start _ MonospacePar   = "<font size=\"3\"><div><pre style=\"line-height: 125%\">"
          html End   _ MonospacePar   = "</pre></div></font>"
          html Start _ Monospace      = "<font size=\"3\"><span style=\"font-family: monospace\">"
          html End   _ Monospace      = "</span></font>"
          html Start _ DiffMain       = "<div style=\"background: #DCDCFF; color: #000080; font-weight: bold; font-family: monospace\">"
          html End   _ DiffMain       = "</div>"
          html Start _ DiffMainExtra  = "<div style=\"background: #DCDCFF; color: #000080; font-family: monospace\">"
          html End   _ DiffMainExtra  = "</div>"
          html Start _ DiffRemove     = "<div style=\"background: #FFE0E0; font-family: monospace\">"
          html End   _ DiffRemove     = "</div>"
          html Start _ DiffAdd        = "<div style=\"background: #E0FFE0; font-family: monospace\">"
          html End   _ DiffAdd        = "</div>"
          html Start (t, _) DiffRemoveFile = diffStartFile False t "#FFE0E0"
          html End   _ DiffRemoveFile = diffEndFile
          html Start (t, _) DiffAddFile    = diffStartFile True t "#E0FFE0"
          html End   _ DiffAddFile    = diffEndFile
          html Start _ DiffHunkHeader = "<div style=\"background: #E0E0E0; font-weight: bold; font-family: monospace\">"
          html End   _ DiffHunkHeader = "</div>"
          html Start _ DiffUnchanged  = "<div style=\"background: #F8F8F5; font-family: monospace\">"
          html End   _ DiffUnchanged  = "</div>"
          html Start _ Underline      = "<div style=\"text-decoration: underline\">"
          html End   _ Underline      = "</div>"
          html Start _ (Emphesis _)   = "<div style=\"font-weight: bold\">"
          html End   _ (Emphesis _)   = "</div>"
          html Start _ List           = "<ul>"
          html End   _ List           = "</ul>"
          html Start _ ListItem       = "<li>"
          html End   _ ListItem       = "</li>"
          html Start _ Table          = "<blockqoute><table cellpadding=\"2\">"
          html End   _ Table          = "</table></blockqoute>"
          html Start _ (TableRow _)   = "<tr>"
          html End   _ (TableRow _)   = "</tr>"
          html Start _ (TableCellPad i) = "<td width=\"" +@ showT i +@ "\">"
          html End   _ (TableCellPad _) = "</td>"
          html Start _ (TableCol _ i)  = "<td colspan=\"" +@ showT i +@ "\">"
          html End   _ (TableCol _ _)  = "</td>"
          html Start _ (Link t)       = linkStart t
          html End   _ (Link _)       = "</a>"
          html Start (_, m) Inverse        = if | DiffRemove `elem` m -> "<span style=\"background: #F8CBCB;\">"
                                                | DiffAdd    `elem` m -> "<span style=\"background: #A6F3A6;\">"
                                                | otherwise           -> ""
          html End   (_, m) Inverse        = if | DiffRemove `elem` m -> "</span>"
                                                | DiffAdd    `elem` m -> "</span>"
                                                | otherwise           -> ""
          html Start _ Dark           = "<span style=\"color: #a0a0a0\">"
          html End   _ Dark           = "</span>"
          html Start _ Footer         = "<div height=\"20\">&nbsp;</div><div style=\"color: #b0b0b0; font-size: 10px\">"
          html End   _ Footer         = "</div>"

flistToText :: FList -> Text
flistToText l = T.concat $ map f l
    where f (t, _) = t

flistToANSI :: FList -> Text
flistToANSI = flistToText -- TODO

test :: IO ()
test = do
    let Right v1 = ansiToFList "bla\x1b[7mbold\x1b[27mokay, foo\x1b[7mbold\x1b[27mrrr"
    print $ combineFLists v1 v1 == Right v1
    let v2 = [("blabo", []), ("ldokay, fooboldr", [Emphesis 0]), ("rr", [])]
    let v3 = Right [("bla",[]),("bo",[Inverse]),("ld",[Inverse,Emphesis 0]),("okay, foo",[Emphesis 0]),("bold",[Inverse,Emphesis 0]),("r",[Emphesis 0]),("rr",[])]
    print $ combineFLists v1 v2 == v3
    print $ highlightDiff "diff \nindex \n--- \n+++ \n@@ bla\n x\n-x\n+x\n @@ bla\ndiff \n"
    return ()
