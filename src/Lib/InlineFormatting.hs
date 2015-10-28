{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.InlineFormatting
       (highlightDiff, combineFLists, FList, Fragment(..),
        flistToInlineStyleHtml, flistToANSI, test, Format(..), mkForm,
        mkFormS, mkPlain, fshow,
        flistToText, highlightText, highlightMonospace) where

------------------------------------------------------------------------------------
import qualified Data.ByteString               as BS
import           Data.ByteString.Search        (indices)
import           Data.Foldable                 (toList)
import           Data.Sequence                 (Seq, (|>), (<|), (><),
                                                ViewR(..), ViewL(..))
import qualified Data.Sequence                 as Seq
import           Data.Text                     (Text)
import qualified Data.Char                     as C
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Encoding            as T
import           Text.Blaze.Html               (toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Algorithm.Patience       as DP
------------------------------------------------------------------------------------
import           Lib.Text                      ((+@), showT, safeDecode)
------------------------------------------------------------------------------------

data Format
    = DiffMain
    | DiffMainExtra
    | DiffHunkHeader
    | DiffAdd
    | DiffRemove
    | DiffAddFile Text
    | DiffRemoveFile Text
    | DiffUnchanged
    | Inverse
    | Emphesis
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

type FList = Seq Fragment

data Fragment
    = TPlain !Text
    | TForm  !Format !FList
      deriving (Show, Eq, Ord)

class FShow a where
    fshow' :: a -> [Text]
    fshow :: a -> Text
    fshow = T.unlines . fshow'

instance FShow FList where
    fshow' lst = ["{"] ++ (map ("   " +@) $ concat $ map fshow' (toList lst)) ++ ["}"]

instance FShow Fragment where
    fshow' (TPlain t) = [T.pack $ show t]
    fshow' (TForm f t) =
        case fshow' t of
           []     -> [T.pack $ show f]
           (x:xs) -> (T.concat [T.pack $ show f, " ", x]):xs

instance FShow a => FShow (Either String a) where
    fshow' (Left x) = [T.pack $ show x]
    fshow' (Right x) = fshow' x

mkFormS :: Format -> FList -> Seq Fragment
mkFormS f s = Seq.singleton $ TForm f $ s

mkForm :: Format -> [Fragment] -> Seq Fragment
mkForm f l = mkFormS f $ Seq.fromList l

mkPlain :: Text -> Seq Fragment
mkPlain t = Seq.singleton $ TPlain t

fragmentize :: [(Text, Maybe Format)] -> FList
fragmentize = foldl w Seq.empty
    where
        w s (t, Nothing) =
            let nt = TPlain t
            in case Seq.viewr s of
                EmptyR              -> Seq.singleton nt
                rs :> (TPlain t')   -> rs |> (TPlain $ t' +@ t)
                _  :> (TForm _ _)   -> s  |> nt
        w s (t, Just f) =
            let nf = TForm f $ Seq.singleton (TPlain t)
            in case Seq.viewr s of
                EmptyR              -> Seq.singleton nf
                _  :> (TPlain    _) -> s |> nf
                rs :> (TForm f2 s2) ->
                    if f2 == f
                       then rs |> (TForm f (w s2 (t, Nothing)))
                       else s |> nf

simpleDelimitersToFList :: Format -> BS.ByteString -> BS.ByteString -> Text -> Either String FList
simpleDelimitersToFList format bStart bEnd t = do
    let te = T.encodeUtf8 t
        l = indices bStart te
        r = indices bEnd te
    z <- if length l /= length r
         then Left "unclosed delimiters"
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

        z_set          = assign z_cap      $ Just format
        in_between_set = assign in_between $ Nothing

        m ((s, e), rval) = (safeDecode $ BS.take (e - s) $ BS.drop s te, rval)
        flists = if l == [] || r == []
                     then assign [t] Nothing
                     else map m $ rjoin z_set in_between_set

    return $ fragmentize flists

lineSplit :: Text -> [Text]
lineSplit t = map (\x->T.concat[x,"\n"]) $ T.splitOn ("\n") t

highlightMonospace :: Text -> FList
highlightMonospace t = Seq.singleton (TForm MonospacePar $ Seq.singleton $ TPlain t)

highlightDiff :: Text -> FList
highlightDiff text = mkFormS MonospacePar root
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
                               else (reverse lst) ++ (remarkInBetween xs)
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

highlightText :: Text -> FList
highlightText text = Seq.singleton (TPlain text)

unfoldrE :: (b -> Either String (Maybe (b, Seq a))) -> b -> (Either String (Seq a), b)
unfoldrE f = unfoldr' Seq.empty
    where unfoldr' as b = either (\s -> (Left s, b)) (z as b)  $ f b
          z as b        = maybe (Right as, b)  $ \(b', a) -> unfoldr' (as >< a) b'

--
-- Combination examples:
--
-- ["test", X ["foo", "bar"], "x"]
-- ["te", Y ["stfoo", "ba"], "rx"]
--
-- ["st", X ["foo", "bar"], "x"]
-- [Y ["stfoo", "ba"], "rx"]
--
-- [X ["foo", "bar"], "x"]
-- [Y ["foo", "ba"], "rx"]
--
-- ["te", Y ["st"], X [ Y["foo", "ba"], "r"], "x"]
-- ["te", Y ["st",  X ["foo", "ba"]], X["r"], "x"]
--

combineFLists :: FList -> FList -> Either String FList
combineFLists = root
    where
        root fa' fb' = fst $ unf fa' fb'
        unf fa' fb'  = unfoldrE r (fa', fb')
        rj x         = Right $ Just x
        apf f s      = if Seq.null s  then Seq.empty else Seq.singleton $ TForm f s
        add f xs s   = if Seq.null xs then s         else (TForm f xs) <| s
        r (fa, fb)   =
            case (Seq.viewl fa, Seq.viewl fb) of
                (_       , EmptyL)                   -> Right Nothing
                (EmptyL  , _     )                   -> Right Nothing
                (TPlain tx :< xs, TPlain ty :< ys)   ->
                    case (T.stripPrefix tx ty, T.stripPrefix ty tx) of
                        (Just "",  Just "") -> rj ((xs, ys), Seq.singleton $ TPlain tx)
                        (Just l,   Nothing) -> rj ((xs, TPlain l <| ys), Seq.singleton $ TPlain tx)
                        (Nothing,  Just l)  -> rj ((TPlain l <| xs, ys), Seq.singleton $ TPlain ty)
                        _                   -> Left $ "The texts are not equal" ++ show (tx, ty)
                (TPlain _  :< _,   TForm f lst :< ys) ->
                    case unf fa lst of
                        (Left s,  _) -> Left s
                        (Right s, (a, b)) -> rj ((a, add f b ys), apf f s)
                (TForm f lst :< xs, TPlain _  :< _) ->
                    case unf lst fb of
                        (Left s,  _) -> Left s
                        (Right s, (a, b)) -> rj ((add f a xs, b), apf f s)
                (TForm xf xlst :< xs, TForm yf ylst :< ys) ->
                    case unf xlst ylst of
                        (Left s,  _) -> Left s
                        (Right s, (a, b)) -> rj ((add xf a xs, add yf b ys), apf xf $ apf yf s)

data FormatPos = Start | End
    deriving Eq

seqConcat :: Seq (Seq a) -> Seq a
seqConcat s = foldl (><) Seq.empty s

flistToInlineStyleHtml :: Maybe (Bool -> Text -> Text) ->  FList -> Text
flistToInlineStyleHtml fileURL = root
    where root flist          = T.concat $ toList $ seqConcat $ fmap (crux []) flist
          crux _ (TPlain t)   = Seq.singleton (plain t)
          crux s (TForm f l)  = ((html Start s f) <| (seqConcat (fmap (crux (f:s)) l))) |> (html End s f)
          delink x            = T.replace "://" ":/&#8203;/" $ T.replace "." "&#8203;." x
          plain t             = T.concat $ map delink $ TL.toChunks $ renderHtml $ toHtml t
          linkStart h         = T.concat ["<a href=\"" , h, "\" style=\"text-decoration: none\">"]

          diffStartFile n t color = T.concat [maybe "" (\f -> linkStart (f n (T.drop 6 t))) fileURL,
                                            "<div style=\"background: ", color, "; font-family: monospace\">" ]
          diffEndFile             = T.concat ["</div>", maybe "" (const "</a>") fileURL]

          html Start _ (DiffRemoveFile t) = diffStartFile False t "#FFE0E0"
          html End   _ (DiffRemoveFile _) = diffEndFile
          html Start _ (DiffAddFile t)    = diffStartFile True t "#E0FFE0"
          html End   _ (DiffAddFile _)    = diffEndFile

          html Start _ (Link t)       = linkStart t
          html End   _ (Link _)       = "</a>"

          html Start m Inverse        = if | DiffRemove `elem` m -> "<span style=\"background: #F8C2C2;\">"
                                           | DiffAdd    `elem` m -> "<span style=\"background: #A6F3A6;\">"
                                           | otherwise           -> ""
          html End   m Inverse        = if | DiffRemove `elem` m -> "</span>"
                                           | DiffAdd    `elem` m -> "</span>"
                                           | otherwise           -> ""

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
          html Start _ DiffHunkHeader = "<div style=\"background: #E0E0E0; font-weight: bold; font-family: monospace\">"
          html End   _ DiffHunkHeader = "</div>"
          html Start _ DiffUnchanged  = "<div style=\"background: #F8F8F5; font-family: monospace\">"
          html End   _ DiffUnchanged  = "</div>"
          html Start _ Underline      = "<div style=\"text-decoration: underline\">"
          html End   _ Underline      = "</div>"
          html Start _ Emphesis       = "<div style=\"font-weight: bold\">"
          html End   _ Emphesis       = "</div>"
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
          html Start _ Dark           = "<span style=\"color: #a0a0a0\">"
          html End   _ Dark           = "</span>"
          html Start _ Footer         = "<div height=\"20\">&nbsp;</div><div style=\"color: #b0b0b0; font-size: 10px\">"
          html End   _ Footer         = "</div>"

flistToText :: FList -> Text
flistToText = root
    where root l           = T.concat $ toList $ seqConcat $ fmap crux l
          crux (TPlain t)  = Seq.singleton t
          crux (TForm _ l) = seqConcat (fmap crux l)

flistToANSI :: FList -> Text
flistToANSI = flistToText -- TODO

test :: IO ()
test = do
    let d1 format = simpleDelimitersToFList format "[" "]"
    let Right v1 = d1 Inverse "test [foo boo] x [hello] world"
    let Right v2 = d1 Monospace "te[st foo boo x] hello[ w]orld"
    -- T.putStrLn $ fshow v1
    -- T.putStrLn $ fshow v2
    T.putStrLn $ fshow $ combineFLists v1 v2
    -- print $ highlightDiff "diff \nindex \n--- \n+++ \n@@ bla\n x\n-x\n+x\n @@ bla\ndiff \n"
    return ()
