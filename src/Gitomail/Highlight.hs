{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Gitomail.Highlight
    ( highlight
    , captureToFList
    , getHighlighter
    ) where

------------------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import           Control.Monad          (forM_)
import qualified Data.DList             as DList
----
import           Gitomail.Gitomail
import           Lib.SourceHighlight
import           Lib.LiftedPrelude
import           Lib.Regex
import           Lib.Formatting
import           Lib.AnsiFormatting
------------------------------------------------------------------------------------

captureToFList :: Capture Element a -> FList
captureToFList = root
   where root :: Capture Element a -> FList
         root c@(CapText _)    = DList.singleton $ nonListToElem c
         root (CapList s)      = DList.fromList $ map nonListToElem $ DList.toList s
         root c@(CapGroup _ _) = DList.singleton $ nonListToElem c

         nonListToElem :: Capture Element NonList -> Fragment
         nonListToElem (CapText t) = TPlain t
         nonListToElem (CapGroup e c) = TForm (Style e) $ captureToFList c

getHighlighter :: Text -> (Text -> Capture Element AList)
getHighlighter s | ".hs"  `T.isSuffixOf` s   = haskellMatcher
                 | ".c"   `T.isSuffixOf` s   = clangMatcher
                 | ".h"   `T.isSuffixOf` s   = clangMatcher
                 | ".cc"  `T.isSuffixOf` s   = clangMatcher
                 | ".hh"  `T.isSuffixOf` s   = clangMatcher
                 | ".cpp" `T.isSuffixOf` s   = clangMatcher
                 | ".hpp" `T.isSuffixOf` s   = clangMatcher
                 | ".hxx" `T.isSuffixOf` s   = clangMatcher
                 | ".cxx" `T.isSuffixOf` s   = clangMatcher
                 | otherwise                 = nullMatcher

highlight :: (MonadGitomail m) => Maybe Text -> m ()
highlight s' = do
    let style =
            case s' of
               Nothing -> undefined -- TODO: assume diff highlighting
               (Just s) -> getHighlighter s

    r <- liftIO $ T.getContents
    let _debug :: (MonadGitomail m) => m ()
        _debug =
            forM_ (splitToLinesArray $ captureToFList $ style r) $
                liftIO . putStrLn . show

    -- liftIO $ T.putStr $ ansiFormatting $ captureToFList $ style r
    _debug

    return ()
