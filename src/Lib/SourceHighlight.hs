{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Lib.SourceHighlight where

------------------------------------------------------------------------------------
import qualified Data.ByteString            as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
----
import           Lib.Formatting             as F
import           Lib.Lexer.Lexer            (Token (..), TokenClass (..),
                                             alexMonadScan, runAlex, clang,
                                             alexSetStartCode, haskell)
import           Lib.SourceHighlight.Data
------------------------------------------------------------------------------------

defaultTheme :: (Int -> Int -> Int -> t) -> Element -> t
defaultTheme code = root
     where root Keyword     = code 0xa7 0x1d 0x5d
           root Comment     = code 0x96 0x98 0x96
           root String      = code 0x40 0x70 0xff
           root Char        = code 0xa0 0x70 0x00
           root ImportLine  = code 0xff 0xff 0xff
           root TopLevel    = code 0xa0 0xa0 0x40
           root Type        = code 0x40 0xa0 0xa0
           root Call        = code 0x00 0xa0 0xa0
           root Number      = code 0xa0 0x30 0x00
           root Special     = code 0xf7 0x7d 0xf7
           root _           = code 0xff 0xff 0xff

parseWithAlex :: Int -> ([(BL8.ByteString, Element)] -> [(BL8.ByteString, Element)]) -> Text -> Either String FList
parseWithAlex s p t =
    let getTokens bs'     = runAlex bs' (alexSetStartCode s >> loop [])
        bs                = BL8.fromChunks [ T.encodeUtf8 t ]
        toText txt        = T.decodeUtf8 $ BS8.concat $ BL8.toChunks txt
        loop s' = do
            Token cls bs' <- alexMonadScan
            case cls of
                TokenEOF -> return s'
                TokenDemark e -> loop $ (bs', e):s'

    in case getTokens bs of
        Left err -> Left err
        Right ok -> Right $ F.fragmentize
                          $ map (\(bs', e) -> (toText bs', Just $ Style e)) $ p ok

nullMatcher,
  haskellMatcher,
  clangMatcher :: Text -> Either String FList

nullMatcher t = Right $ F.highlightText t

haskellMatcher = parseWithAlex haskell reverse

clangMatcher = parseWithAlex clang (p [])
    where p r []                            = r
          p r (("(", f):(x, Identifier):xs) = p ((x, Call):(("(", f):r)) xs
          p r (x:xs)                        = p (x:r) xs