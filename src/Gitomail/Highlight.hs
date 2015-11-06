{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Gitomail.Highlight
    ( highlight
    , getHighlighter
    ) where

------------------------------------------------------------------------------------
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
----
import           Gitomail.Gitomail
import           Lib.Formatting             as F
import           Lib.SourceHighlight
import           Lib.LiftedPrelude
import           Lib.AnsiFormatting
------------------------------------------------------------------------------------

getHighlighter' :: Text -> Text -> Either String FList
getHighlighter' s | ".hs"  `T.isSuffixOf` s   = haskellMatcher
                  | ".c"   `T.isSuffixOf` s   = clangMatcher
                  | ".h"   `T.isSuffixOf` s   = clangMatcher
                  | ".cc"  `T.isSuffixOf` s   = clangMatcher
                  | ".hh"  `T.isSuffixOf` s   = clangMatcher
                  | ".cpp" `T.isSuffixOf` s   = clangMatcher
                  | ".hpp" `T.isSuffixOf` s   = clangMatcher
                  | ".hxx" `T.isSuffixOf` s   = clangMatcher
                  | ".cxx" `T.isSuffixOf` s   = clangMatcher
                  | otherwise                 = nullMatcher

getHighlighter :: Text -> Text -> FList
getHighlighter s t =
    case (getHighlighter' s) t of
        Left _ -> F.highlightText t
        Right ok -> ok

highlight :: (MonadGitomail m) => Maybe Text -> m ()
highlight s' = do
    let style =
            case s' of
               Nothing -> undefined -- TODO: assume diff highlighting
               (Just s) -> getHighlighter' s

    r <- liftIO $ T.getContents

    case style r of
        Left err -> liftIO $ putStrLn $ show err
        Right s -> liftIO $ T.putStr $ ansiFormatting $ s
