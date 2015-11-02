{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Lib.InlineFormatting
 where

------------------------------------------------------------------------------------
import qualified Data.DList                    as DList
import           Data.Foldable                 (toList)
import           Data.Text                     (Text)
import qualified Data.Text                     as T

import qualified Data.Text.Lazy                as TL

import           Text.Blaze.Html               (toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Printf                   (printf)
------------------------------------------------------------------------------------
import           Lib.Text                      ((+@), showT)
import           Lib.DList                     (dlistConcat)
import           Lib.SourceHighlight           (defaultTheme, Element(Ignore))
import           Lib.Formatting
------------------------------------------------------------------------------------

data FormatPos = Start | End
    deriving Eq

flistToInlineStyleHtml :: Maybe (Bool -> Text -> Text) -> FList -> Text
flistToInlineStyleHtml fileURL = root
    where root flist          = T.concat $ toList $ dlistConcat $ fmap (crux []) flist
          crux _ (TPlain t)   = DList.singleton (plain t)
          crux s (TForm f l)  = ((html Start s f) `DList.cons` (dlistConcat (fmap (crux (f:s)) l))) `DList.snoc` (html End s f)
          delink x            = T.replace "://" ":/&#8203;/" $ T.replace "." "&#8203;." x
          plain t             = T.concat $ map delink $ TL.toChunks $ renderHtml $ toHtml t
          linkStart h         = T.concat ["<a href=\"" , h, "\" style=\"text-decoration: none\">"]

          diffStartFile n t color = T.concat [maybe "" (\f -> linkStart (f n t)) fileURL,
                                            "<div style=\"background: ", color, "; font-family: monospace\">" ]
          diffEndFile             = T.concat ["</div>", maybe "" (const "</a>") fileURL]

          html Start _ (DiffRemoveFile t) = diffStartFile False t "#FFE0E0"
          html End   _ (DiffRemoveFile _) = diffEndFile
          html Start _ (DiffAddFile t)    = diffStartFile True t "#E0FFE0"
          html End   _ (DiffAddFile _)    = diffEndFile

          html Start _ (Link t)       = linkStart t
          html End   _ (Link _)       = "</a>"

          html Start m Mark           = if | DiffRemove `elem` m -> "<span style=\"background: #F8C2C2;\">"
                                           | DiffAdd    `elem` m -> "<span style=\"background: #A6F3A6;\">"
                                           | otherwise           -> ""
          html End   m Mark           = if | DiffRemove `elem` m -> "</span>"
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
          html Start _ (Color r g b)  = "<span style=\"color: #" +@ hex r +@ hex g +@ hex b +@ "\">"
              where hex i = T.pack (printf "%02x" i)
          html End   _ (Color _ _ _)  = "</span>"
          html Start _ List           = "<ul>"
          html End   _ List           = "</ul>"
          html Start _ ListItem       = "<li>"
          html End   _ ListItem       = "</li>"
          html _     _ (Style Ignore) = ""
          html Start _ (Style s)      = "<span style=\"color: #" +@ (defaultTheme code) s +@ "\">"
          html End   _ (Style _)      = "</span>"
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

          darker :: Int -> Int
          darker x = x - (x `div` 4)

          code :: Int -> Int -> Int -> Text
          code r g b       = T.pack (printf "%02x%02x%02x" (darker r) (darker g) (darker b))
