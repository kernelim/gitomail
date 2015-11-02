{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving        #-}

module Lib.Regex
    ( matchWhole
    , (+/)
    , (=~+)
    , AList
    , NonList
    , cap
    , capt
    , choice
    , con
    , describeMatches
    , list
    , matchCaptures
    , matchLists
    , matchLists'
    , optcap
    , reg
    , showf
    , singleText
    , test
    , toIndexes
    , toRegex
    , Capture(..)
    ) where

------------------------------------------------------------------------------------
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import           Text.Regex.TDFA             (MatchArray, getAllMatches,
                                              MatchOffset, MatchLength)
import           Text.Regex.TDFA.Text        ()
import           Text.Regex.TDFA.Common      (Regex, CompOption(..), ExecOption(..))
import           Text.Regex.Base.RegexLike   (RegexLike)
import           Text.Regex.Base             (RegexMaker, RegexContext, makeRegexOpts,
                                              match)
import           Data.Maybe                  (catMaybes)
import           Data.DList                  (DList)
import qualified Data.DList                  as DList
import           Data.Foldable               (toList)
import           Data.List                   (intersperse)
import qualified Data.Array                  as Array
import           Data.Array                  (Array, listArray)
import           Control.Monad.State         (evalState, State, get, put)
------------------------------------------------------------------------------------
import           Lib.Text                    (textToAText, subAText)
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

data NonList = NonList  deriving Show
data AList  = AList     deriving Show

class Show a => CaptureConstrains a where

instance CaptureConstrains AList where
instance CaptureConstrains NonList where

data Capture a b where
    CapText  :: Text                            -> Capture a NonList
    CapList  :: DList (Capture a NonList)       -> Capture a AList
    CapGroup :: (CaptureConstrains b) =>
                 a -> (Capture a b)             -> Capture a NonList

deriving instance Show a => Show (Capture a b)

--
--

class Adder a b c where
    (+/) :: Capture a b -> Capture a c -> Capture a AList

instance Adder a AList AList where
    (+/) (CapList s) (CapList t) = CapList $ s `DList.append` t

instance Adder a NonList    AList where
    (+/) x                (CapList t) = CapList $ x `DList.cons` t

instance Adder a AList   NonList where
    (+/) (CapList t)      x           = CapList $ t `DList.snoc` x

instance Adder a NonList    NonList   where
    (+/) (CapText x)      (CapText y) = CapList $ DList.singleton $ CapText $ T.concat [x, y]
    (+/) a                b           = CapList $ DList.fromList [a, b]

con :: Foldable r => r (Capture t AList) -> Capture t AList
con l = foldl1 (+/) $ toList $ l

infixl 3 +/

singleText :: Text -> Capture a AList
singleText t = CapList $ DList.singleton $ reg t

reg :: forall a. Text -> Capture a NonList
reg  t   = CapText t

capt :: forall a. a -> Text -> Capture a NonList
capt a t = CapGroup a (reg t)

cap :: forall a b.
        CaptureConstrains b =>
        a -> Capture a b -> Capture a NonList
cap  a c = CapGroup a c

optcap :: forall a b.
        CaptureConstrains b =>
        a -> Capture a b -> Capture a AList
optcap  a c = CapGroup a c +/ reg "?"

list :: forall a. [Capture a NonList] -> Capture a AList
list l   = CapList $ DList.fromList l

choice :: forall a. [Capture a NonList] -> Capture a AList
choice r = list $ intersperse (reg "|") r

class ShowF a where
    showf :: a -> String

instance Show a => ShowF (Capture a b) where
    showf (CapText t)    = T.unpack t
    showf (CapList  s)   = concat $ map showf (toList s)
    showf (CapGroup x s) = "(?P<" ++ show x ++ ">" ++ (showf s) ++ ")"

toRegex :: Capture a b -> Text
toRegex (CapText t)    = t
toRegex (CapList s)    = T.concat $ map toRegex (toList s)
toRegex (CapGroup _ s) = T.concat $ ["(", toRegex s, ")"]

toIndexes :: Capture a b -> Array Int a
toIndexes = arr
   where root :: Capture a b -> [a]
         root (CapText   _) = []
         root (CapList   s) = concat $ map root $ toList s
         root (CapGroup  a s) = a:(root s)
         arr x = listArray (1, length $ root x) $ root x

matchLists' :: Regex -> Text -> Array Int MatchArray
matchLists' r t = getAllMatches (t =~* r)

matchLists :: Capture t b -> Text -> Array Int MatchArray
matchLists c t = matchLists' (compile' $ toRegex c) t

describeMatches :: Text -> Array Int MatchArray -> Text
describeMatches t ml = root
    where range start len  = T.take len $ T.drop start t
          root             = T.concat $ lst
          lst              = concat $ intersperse ["\n"] $ map one $ Array.elems ml
          one matches      = intersperse ", " $ catMaybes $ map asc $ (Array.assocs matches)
          asc (_, (-1, _)) = Nothing
          asc (i, (s, e))  = Just $ T.concat $ (replicate i "  ") ++
                               [T.pack $ show i ++ ": " ++ (show $ range s e), "\n"]

matchCaptures :: (Show t, Show b) => Capture t b -> Text -> Capture t AList
matchCaptures c t = con $ root
    where
        maxT                = T.length t
        atext               = subAText $ textToAText t
        range start len     = atext start len
        matches             = matchLists c t
        root                = DList.unfoldr iterTop (0, Array.elems matches)

        iterTop (p, [])     = if p >= T.length t
                                 then Nothing
                                 else Just $ (list [ CapText $ T.drop p t], (maxT, []))
        iterTop (p, xs@(matched:xrs)) =
            case Array.elems matched of
                []                    -> Nothing
                ((start, len):nxs)    -> if p < start
                                             then Just $ (list [ CapText $ range p (start - p)], (start, xs))
                                             else let r = evalState (foo c) ((start, start + len), nxs)
                                                   in Just $ (fst r, (start + len, xrs))

        wrapPost :: (Show a) => Int -> Int -> Maybe (Capture a NonList)
        wrapPost rend end = if rend < end      then Just (CapText $ range rend    (end - rend)) else Nothing

        wrapPre :: (Show a) => Int -> Int -> [Capture a NonList]
        wrapPre rstart start = if rstart > start  then [(CapText $ range start (rstart - start))] else []

        foo :: (Show a, Show b) => Capture a b -> State ((Int, Int), [(MatchOffset, MatchLength)]) ((Capture a AList), Bool)
        foo (CapText   _)   = return $ (CapList DList.empty, False)

        foo (CapList   s)   = do
            r <- mapM foo $ toList s
            return $ (con $ map fst r, or (map snd r))

        foo (CapGroup  a s) = do
            ((start, end), mxs) <- get
            case mxs of
                [] ->            do  return (CapList DList.empty, False)

                ((-1, _):nxs) -> do  put ((start, end), nxs)
                                     _ <- foo s
                                     return (CapList DList.empty, False)

                ((rstart, rlen):nxs) -> do
                    let pre = wrapPre rstart start
                    let rend = rstart + rlen
                    put $ ((rstart, rend), nxs)

                    (r, matched) <- foo s
                    case matched of
                        False -> do
                            (_, nxs') <- get
                            put $ ((rend, rend), nxs')
                            return $ (list $ pre ++ [(CapGroup a (CapText $ range rstart rlen))], True)
                        True -> do
                            ((_, rend'), _) <- get
                            let r' = case wrapPost rend' end of Nothing -> r ; Just post -> r +/ post
                            return $ (list $ pre ++ [(CapGroup a r')], True)

data Test = FirstName | SurName | FullName | SomeName
    deriving Show

test :: IO ()
test = do
    let example :: Capture Test AList
        example = list [ reg "[Hh]ello ",
                         cap FullName (reg "Mr. " +/ capt FirstName "[a-zA-z_]+" +/ reg " " +/ capt SurName "[a-zA-z_]+")]
     in do
          print (getAllMatches (("test bbaaaabb bbb" ::Text) =~+ ("(a+)|(b+)|bbaaaabb" :: Text)) :: Array Int MatchArray)
          print example
          print $ showf example
          let s = "Check this out. Hello Mr. a b, and hello Mr. c d."
          print $ matchLists example s
          let m = matchCaptures example s
          print $ m
          print $ showf m
          print $ toIndexes example

    let example :: Capture Test AList
        example = list [ cap SomeName (choice [capt FirstName "first", capt SurName "last"]) ]
     in print $ showf $ matchCaptures example "These are first and last names"

    let example :: Capture Test AList
        example = choice [ cap SomeName (capt FirstName "first"), capt SurName "last" ]
        t3       = "These are first and last names"
     in do T.putStr $ describeMatches t3 (matchLists example t3)
           print $ showf example
           print $ showf $ matchCaptures example t3

    let example :: Capture Test NonList
        example = cap FullName $ list [ reg "\"" , cap SomeName (reg "[^\\\"]*[\\].") , reg "*[^\\\"]*\"" ]
        t3      = "These are \"first\" and \"la\\nst\" names"
     in do T.putStr $ describeMatches t3 (matchLists example t3)
           print $ showf example
           print $ showf $ matchCaptures example t3

    return ()
