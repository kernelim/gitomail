{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.InlineFormatting where

------------------------------------------------------------------------------------
import qualified Data.DList                    as DList
import           Data.Foldable                 (toList)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Text.Blaze.Html               (toHtml)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
----
import           Lib.DList                     (dlistConcat)
import           Lib.Text                      (showT, (+@))
import           Lib.Formatting
------------------------------------------------------------------------------------

data FormatPos = Start | End
    deriving Eq

flistToInlineStyleHtml :: FList -> Text
flistToInlineStyleHtml = root
    where root flist          = T.concat $ toList $ (before `DList.cons` (dlistConcat $ fmap (crux []) flist)) `DList.snoc` after
          crux _ (TPlain t)   = DList.singleton (plain t)
          crux s (TForm f l)  = ((html Start s f) `DList.cons` (dlistConcat (fmap (crux (f:s)) l))) `DList.snoc` (html End s f)
          delink x            = T.replace "://" ":/&#8203;/" $ T.replace "." "&#8203;." x
          plain t             = T.concat $ map delink $ TL.toChunks $ renderHtml $ toHtml t

          before              = ""
          after               = ""

          linkStart h         = T.concat ["<a href=\"" , h, "\" style=\"text-decoration: none\">"]

          html Start _ MonospacePar   = "<font size=\"3\"><div><pre style=\"line-height: 125%\">"
          html End   _ MonospacePar   = "</pre></div></font>"
          html Start _ Monospace      = "<font size=\"3\"><span style=\"font-family: monospace\">"
          html End   _ Monospace      = "</span></font>"
          html Start _ Underline      = "<div style=\"text-decoration: underline\">"
          html End   _ Underline      = "</div>"
          html Start _ LineThrough    = "<div style=\"text-decoration: line-through\">"
          html End   _ LineThrough    = "</div>"
          html Start _ Emphesis       = "<div style=\"font-weight: bold\">"
          html End   _ Emphesis       = "</div>"
          html Start _ List           = "<ul>"
          html End   _ List           = "</ul>"
          html Start _ ListItem       = "<li>"
          html End   _ ListItem       = "</li>"
          html Start _ Table          = "<blockqoute><table cellpadding=\"2\">"
          html End   _ Table          = "</table></blockqoute>"
          html Start _ (Link t)       = linkStart t
          html End   _ (Link _)       = "</a>"
          html Start _ (TableRow)     = "<tr>"
          html End   _ (TableRow)     = "</tr>"
          html Start _ (TableCellPad i) = "<td width=\"" +@ showT i +@ "\">"
          html End   _ (TableCellPad _) = "</td>"
          html Start _ (TableCol i)   = "<td colspan=\"" +@ showT i +@ "\">"
          html End   _ (TableCol _)   = "</td>"
          html Start _ Dark           = "<span style=\"color: #a0a0a0\">"
          html End   _ Dark           = "</span>"
          html Start _ Footer         = "<div height=\"20\">&nbsp;</div><div style=\"color: #b0b0b0; font-size: 10px\">"
          html End   _ Footer         = "</div>"
          html Start _ _              = "<span>"
          html End   _ _              = "</span>"
