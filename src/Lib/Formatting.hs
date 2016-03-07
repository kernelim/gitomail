{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Formatting
       (Format(..), Element(..), FList, Fragment(..),
       mkPlain, mkForm, mkFormS, flistToText, fshow) where

------------------------------------------------------------------------------------
import           Data.DList    (DList)
import qualified Data.DList    as DList
import           Data.Foldable (toList)
import           Data.Text     (Text)
import qualified Data.Text     as T
----
import           Lib.Text      ((+@))
------------------------------------------------------------------------------------

data Element
    = Ignore
    | FreeForm !Text
    deriving (Show, Eq, Ord)

data Format
    = Mark
    | Emphesis
    | MonospacePar
    | Monospace
    | Underline
    | Dark
    | List
    | ListItem
    | Table
    | TableRow
    | TableCol Int
    | TableCellPad Int
    | Link Text
    | Footer
    | Style Element
      deriving (Show, Eq, Ord)

type FList = DList Fragment

data Fragment
    = TPlain {-# UNPACK #-} !Text
    | TForm  !Format FList
      deriving (Show, Eq, Ord)

class FShow a where
    fshow' :: a -> [Text]
    fshow :: a -> Text
    fshow = T.unlines . fshow'

instance FShow FList where
    fshow' lst = case concat $ map fshow' (toList lst) of
        [x] -> ["{ " +@ x +@ " }"]
        xs  -> ["{"] ++ (map ("   " +@) $ xs) ++ ["}"]

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

flistToText :: FList -> Text
flistToText = root
    where root l           = T.concat $ concat $ map crux $ toList l
          crux (TPlain t)  = [t]
          crux (TForm _ l) = concat $ map crux $ toList l
