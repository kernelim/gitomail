{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Formatting
       (combineFLists, FList, Fragment(..),
        test, Format(..), mkForm, clearFormatting,
        mkFormS, mkPlain, fshow, fragmentize,
        flistToText, highlightText, highlightMonospace,
        splitToLinesArray) where

------------------------------------------------------------------------------------
import qualified Data.ByteString               as BS
import           Data.ByteString.Search        (indices)
import           Data.Foldable                 (toList)
import           Data.List                     (groupBy)
import           Data.Either                   (isLeft, partitionEithers)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Text.Encoding            as T
import qualified Data.Array                    as A
import           Data.DList                    (DList)
import qualified Data.DList                    as DList
------------------------------------------------------------------------------------
import           Lib.Text                      ((+@), lineSplit)
import           Lib.DList                     (unfoldrEDL, ViewL(..),
                                                viewl)
import           Lib.SourceHighlight.Data      (Element)
------------------------------------------------------------------------------------

data Format
    = DiffMain
    | DiffMainExtra
    | DiffHunkHeader
    | DiffAdd
    | DiffRemove
    | DiffSlash
    | DiffAddFile Text
    | DiffRemoveFile Text
    | DiffUnchanged
    | Mark
    | Emphesis
    | MonospacePar
    | Monospace
    | Underline
    | List
    | ListItem
    | Table
    | TableRow
    | TableCol Int
    | TableCellPad Int
    | Link Text
    | Footer
    | Style Element
    | Dark
      deriving (Show, Eq, Ord)

type FList = DList Fragment

data Fragment
    = TPlain {-# UNPACK #-} !Text
    | TForm  !Format FList
      deriving (Show, Eq, Ord)

toFList :: Fragment -> FList
toFList x@(TPlain _)    = DList.singleton x
toFList (TForm _ flist) = flist

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

mkFormS :: Format -> FList -> FList
mkFormS f s = DList.singleton $ TForm f $ s

mkForm :: Format -> [Fragment] -> FList
mkForm f l = mkFormS f $DList.fromList l

mkPlain :: Text -> FList
mkPlain t = DList.singleton $ TPlain t

clearFormatting :: [(Text, Format)] -> FList
clearFormatting tl = DList.singleton $ TPlain $ T.concat $ map fst tl

fragmentize :: [(Text, Maybe Format)] -> FList
fragmentize = root
    where
        root l             = DList.fromList $ map g $ groupBy i l
        i (_, x) (_, y)    = x == y
        g l                =
            case snd $ head l of
                Nothing -> TPlain xl
                Just f  -> TForm f $ DList.singleton (TPlain xl)
            where xl = T.concat $ map fst l

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

        m ((s, e), rval) = (T.decodeUtf8 $ BS.take (e - s) $ BS.drop s te, rval)
        flists = if l == [] || r == []
                     then assign [t] Nothing
                     else map m $ rjoin z_set in_between_set

    return $ fragmentize flists

highlightMonospace :: Text -> FList
highlightMonospace t = DList.singleton (TForm MonospacePar $ DList.singleton $ TPlain t)


highlightText :: Text -> FList
highlightText text = DList.singleton (TPlain text)

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
        unf fa' fb'  = unfoldrEDL r (fa', fb')
        rj x         = Right $ Just x
        apf f s      = if DList.toList s == []  then DList.empty else DList.singleton $ TForm f s
        add f xs s   = if DList.toList xs == [] then s           else (TForm f xs) `DList.cons` s
        r (fa, fb)   =
            case (viewl fa, viewl fb) of
                (_       , EmptyL)                   -> Right Nothing
                (EmptyL  , _     )                   -> Right Nothing

                (TPlain tx :<< xs, TPlain ty :<< ys)   ->
                    case (T.stripPrefix tx ty, T.stripPrefix ty tx) of
                        (Just "",  Just "") -> rj ((xs, ys), DList.singleton $ TPlain tx)
                        (Just l,   Nothing) -> rj ((xs, TPlain l `DList.cons` ys), DList.singleton $ TPlain tx)
                        (Nothing,  Just l)  -> rj ((TPlain l `DList.cons` xs, ys), DList.singleton $ TPlain ty)
                        _                   -> Left $ "The texts are not equal" ++ show (tx, ty)
                (TPlain _  :<< _,   TForm f lst :<< ys) ->
                    case unf fa lst of
                        (Left s,  _) -> Left s
                        (Right s, (a, b)) -> rj ((a, add f b ys), apf f s)
                (TForm f lst :<< xs, TPlain _  :<< _) ->
                    case unf lst fb of
                        (Left s,  _) -> Left s
                        (Right s, (a, b)) -> rj ((add f a xs, b), apf f s)
                (TForm xf xlst :<< xs, TForm yf ylst :<< ys) ->
                    case unf xlst ylst of
                        (Left s,  _) -> Left s
                        (Right s, (a, b)) -> rj ((add xf a xs, add yf b ys), apf xf $ apf yf s)

splitToLinesArray :: FList -> A.Array Int FList
splitToLinesArray = root
    where root flist                   = A.listArray (1, n) l
               where l = case onFrag (TForm List flist) of
                             Left x    -> [DList.singleton x]
                             Right lst -> map snd $ reduce $ map (\(x, a) -> (x, toFList a)) $ lst
                     n = length l
                     reduce ((0, a):(x, b):xs) = reduce $ (x, (a `DList.append` b)):xs
                     reduce (x:xs)             = x : (reduce $ xs)
                     reduce xs                 = xs

          onFlist flist                = map onFrag $ toList flist
          allLefts l                   = and $ map isLeft l
          onFrag x@(TPlain t)          = case lineSplit t of
                                             [_] -> if "\n" `T.isSuffixOf` t
                                                       then Right ([(1, x)]) else Left x
                                             lst -> Right (map (\y -> (if "\n" `T.isSuffixOf` y
                                                                       then 1 else 0, TPlain y)) lst)
          onFrag x@(TForm f flist)     = crux
                  where slist = onFlist flist
                        y lr  = case partitionEithers lr of
                                   (l, []) -> [(0 :: Int, TForm f $ DList.fromList l)]
                                   ([], r) -> map (\(t,s) -> (t, TForm f $ DList.singleton s))
                                               (concat r)
                                   _ -> error "splitToLinesArray"

                        rlist = map y (groupBy (\a b -> isLeft a == isLeft b) slist)
                        crux = if allLefts slist
                                 then Left x
                                 else Right $ concat $ rlist

flistToText :: FList -> Text
flistToText = root
    where root l           = T.concat $ concat $ map crux $ toList l
          crux (TPlain t)  = [t]
          crux (TForm _ l) = concat $ map crux $ toList l

test :: IO ()
test = do
    let d1 format = simpleDelimitersToFList format "[" "]"
    let Right v1 = d1 Mark    "test [foo boo] x [hello] world"
    let Right v2 = d1 Monospace "te[st foo boo x] hello[ w]orld"
    print $ splitToLinesArray $
         DList.fromList [TForm Mark    $ DList.fromList [
                                TForm Monospace $ DList.fromList [
                                        TPlain "bla", TPlain "hello\n" , TPlain "wo",
                                        TPlain "rld\nbla", TPlain "X", TPlain "mult\n\n",
                                        TPlain "bla" ] ] ]
    -- T.putStrLn $ fshow v1
    -- T.putStrLn $ fshow v2
    T.putStrLn $ fshow $ combineFLists v1 v2
    -- print $ highlightDiff "diff \nindex \n--- \n+++ \n@@ bla\n x\n-x\n+x\n @@ bla\ndiff \n"
    return ()
