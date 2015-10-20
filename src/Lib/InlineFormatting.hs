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

data Format
    = DiffMain
    | DiffHunkHeader
    | DiffAdd
    | DiffRemove
    | DiffUnchanged
    | Inverse
    | Emphesis
    | MonospacePar
    | Monospace
    | List
    | ListItem
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
        root (x:xs) = case' "diff "  DiffMain       diff x xs $
                      else' root x xs
        root []     = []

        diff (x:xs) = case' "index " DiffMain       diff x xs $
                      case' "new "   DiffMain       diff x xs $
                      case' "--- "   DiffRemove     diff x xs $
                      case' "+++ "   DiffAdd        hunk x xs $
                      else' diff x xs
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

flistToInlineStyleHtml :: FList -> Text
flistToInlineStyleHtml l = crux
    where crux   = T.concat $ toList $ Seq.zipWith3 r seq'a seq' seq'b
          seq'   = Seq.fromList l
          seq'a  = ("", [])            Seq.<|  (Seq.take (Seq.length seq' - 1) seq')
          seq'b  = (Seq.drop 1 seq')   Seq.|>  ("", [])
          delink x               = T.replace "://" ":/&#8203;/" $ T.replace "." "&#8203;." x
          plain t                = T.concat $ map delink $ TL.toChunks $ renderHtml $ toHtml t
          substract la lb        = filter (not . (`elem` lb)) la
          r (_, a) (t, m) (_, b) = T.concat $ concat [map (html Start m) (am),
                                                      [plain t],
                                                      map (html End m) (reverse rm)]
                                   where am = m `substract` a  -- added to M
                                         rm = m `substract` b  -- remove from M

          html Start _ MonospacePar   = "<font size=\"3\"><div><pre style=\"line-height: 125%\">"
          html End   _ MonospacePar   = "</pre></div></font>"
          html Start _ Monospace      = "<font size=\"3\"><span style=\"font-family: monospace\">"
          html End   _ Monospace      = "</span></font>"
          html Start _ DiffMain       = "<div style=\"background: #DCDCFF; color: #000080; font-weight: bold\">"
          html End   _ DiffMain       = "</div>"
          html Start _ DiffRemove     = "<div style=\"background: #FFE0E0;\">"
          html End   _ DiffRemove     = "</div>"
          html Start _ DiffAdd        = "<div style=\"background: #E0FFE0;\">"
          html End   _ DiffAdd        = "</div>"
          html Start _ DiffHunkHeader = "<div style=\"background: #E0E0E0; font-weight: bold\">"
          html End   _ DiffHunkHeader = "</div>"
          html Start _ DiffUnchanged  = "<div style=\"background: #F8F8F5;\">"
          html End   _ DiffUnchanged  = "</div>"
          html Start _ List           = "<ul>"
          html End   _ List           = "</ul>"
          html Start _ ListItem       = "<li>"
          html End   _ ListItem       = "</li>"
          html Start _ (Link t)       = T.concat ["<a href=\"" , t, "\" style=\"text-decoration: none\">"]
          html End   _ (Link _)       = "</a>"
          html Start s Inverse        = if | DiffRemove `elem` s -> "<span style=\"background: #F8CBCB;\">"
                                           | DiffAdd    `elem` s -> "<span style=\"background: #A6F3A6;\">"
                                           | otherwise           -> ""
          html End   s Inverse        = if | DiffRemove `elem` s -> "</span>"
                                           | DiffAdd    `elem` s -> "</span>"
                                           | otherwise           -> ""
          html Start _ Dark           = "<span style=\"color: #a0a0a0\">"
          html End   _ Dark           = "</span>"
          html Start _ Footer         = "<div style=\"color: #b0b0b0; font-size: 10px\">"
          html End   _ Footer         = "</div>"
          html Start _ Emphesis       = ""
          html End   _ Emphesis       = ""

flistToText :: FList -> Text
flistToText l = T.concat $ map f l
    where f (t, _) = t

flistToANSI :: FList -> Text
flistToANSI = flistToText -- TODO

test :: IO ()
test = do
    let Right v1 = ansiToFList "bla\x1b[7mbold\x1b[27mokay, foo\x1b[7mbold\x1b[27mrrr"
    print $ combineFLists v1 v1 == Right v1
    let v2 = [("blabo", []), ("ldokay, fooboldr", [Emphesis]), ("rr", [])]
    let v3 = Right [("bla",[]),("bo",[Inverse]),("ld",[Inverse,Emphesis]),("okay, foo",[Emphesis]),("bold",[Inverse,Emphesis]),("r",[Emphesis]),("rr",[])]
    print $ combineFLists v1 v2 == v3
    print $ highlightDiff "diff \nindex \n--- \n+++ \n@@ bla\n x\n-x\n+x\n @@ bla\ndiff \n"
    return ()
