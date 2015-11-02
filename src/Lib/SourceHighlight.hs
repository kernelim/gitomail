{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Lib.SourceHighlight where

------------------------------------------------------------------------------------
import           Lib.Regex
import           Data.Text              (Text)
import qualified Data.Text              as T
------------------------------------------------------------------------------------

data Element
    = Keyword
    | String
    | Number
    | Char
    | Type
    | ImportLine
    | TopLevel
    | Call
    | FromPrelude
    | Comment
    | Special
    | Ignore
    deriving (Show, Eq, Ord)

word :: Text -> Capture a NonList
word w = reg $ T.concat ["\\b", w, "\\b" ]

ign = cap Ignore
cstring = cap String $ list
          [ reg "\""
          , ign (reg "[^\\\"]*[\\].")
          , reg "*[^\\\"]*\""
          ]
cchar   = cap Char $ list
          [ reg "'"
          , ign (reg "[^\\']*[\\].")
          , reg "*[^\\']*'"
          ]

cnumber = cap Number $ list
          [ reg "\\b"
          , cap Ignore $ choice [
                capt Ignore "[0-9]+"
              , capt Ignore "[0-9]*[.][0-9]+[0-9EF-]*"
              , capt Ignore "0x[0-9a-fA-F]+"
              ]
          , capt Ignore "U|UL|ULL"
          , reg "?"
          , reg "\\b"
          ]

haskell :: Capture Element AList
haskell = choice
    [ capt Comment "--[^\n]*"
    , keyword
    , cap Type   $ list $ [ reg "\\b" ] ++ idStr "[A-Z]"
    , cap Ignore $ list $ [ reg "\\b" ] ++ idStr "[a-z_]"
    , cstring
    , cchar
    , cnumber
    , cap TopLevel $ list
          [ reg "\n"
          , ign $ choice
              [ keyword
              , ign $ list $ idStr "[a-zA-Z_]"
              ]
          ]
    ]

    where
        idStr start = [
            reg start
          , capt Ignore "['_a-zA-Z0-9]+"
          , reg "*"
          ]

        keyword = cap Keyword $ choice
          [ word "let"
          , word "as"
          , word "case"
          , word "of"
          , word "class"
          , word "data"
          , word "data family"
          , word "data instance"
          , word "default"
          , word "deriving"
          , word "deriving instance"
          , word "do"
          , word "forall"
          , word "foreign"
          , word "hiding"
          , word "if"
          , word "import"
          , word "then"
          , word "else"
          , word "infix"
          , word "infixl"
          , word "infixr"
          , word "instance"
          , word "let"
          , word "in"
          , word "mdo"
          , word "module"
          , word "newtype"
          , word "proc"
          , word "qualified"
          , word "rec"
          , word "type"
          , word "type family"
          , word "type instance"
          , word "where"
          ]

cComment = cap Comment $ list
           [ reg "[/][*]"
           , ign (reg "[^*]+[*][^/]|[^*]*")
           , reg "*[*][/]"
           ]

clang :: Capture Element AList
clang = choice
    [ capt Comment "//[^\n]*"
    , cap Special $ reg "[#]"
    , cComment
    , cstring
    , cap String $ list
          [ reg "<\\b"
          , capt Ignore "[^>]*"
          , reg "\\b>"
          ]
    , cchar
    , cnumber
    , cap Call $ list
          [ reg "\\b"
          , capt Ignore "[a-zA-Z_]+"
          , capt Ignore "[a-zA-Z_0-9']*"
          , reg "\\b\\("
          ]
    , keyword
    ]
    where
        keyword = cap Keyword $ choice
           [ word "auto"
           , word "break"
           , word "case"
           , word "char"
           , word "const"
           , word "continue"
           , word "default"
           , word "do"
           , word "double"
           , word "else"
           , word "enum"
           , word "extern"
           , word "float"
           , word "for"
           , word "goto"
           , word "if"
           , word "int"
           , word "long"
           , word "register"
           , word "return"
           , word "short"
           , word "signed"
           , word "sizeof"
           , word "static"
           , word "struct"
           , word "switch"
           , word "typedef"
           , word "union"
           , word "unsigned"
           , word "void"
           , word "volatile"
           , word "while"

           , word "define"
           , word "undef"
           , word "ifdef"
           , word "include"
           , word "include_once"
           , word "include_other"
           , word "defined"
           , word "elif"

           , word "__VA_ARGS__"
           ]

--
-- Catching import lines, to be reprocessed later:
--
-- , cap ImportLine $ list [
--         reg "\\bimport [^\n]*\n"
--         , ign $ choice [ reg "[ \t]+[^\n]*\n",
--                          reg "--[^\n]*\n",
--                          reg "\n" ]
--         , reg "*"
--       ]

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
           root _           = code 0xa7 0x1d 0x5d

nullMatcher :: Text -> Capture Element AList
nullMatcher = singleText

haskellMatcher :: Text -> Capture Element AList
haskellMatcher = matchCaptures haskell

clangMatcher :: Text -> Capture Element AList
clangMatcher = matchCaptures clang

test :: IO ()
test = do
    -- let r = "((\\bimport [^\n]*\n)|\\bb\nla\\b|\\bboo\\b)" :: Text
    --     t = "hey b\nla boo.\nimport boo bla\n" :: Text
    --  in print $ (t =~+ r :: [[Text]])
    let r = "(([^a]*a([^b]|b[^c]))*)(abc)?(.*)" :: Text
        t = "hello bla bla abc bla\n" :: Text
     in print $ (t =~+ r :: [[Text]])

    let r = "(\"([^\\\"]*[\\].)*[^\\\"]*\")" :: Text
        t = "hel\"lo b\"la bla \"a\\\"bc\" \"a\\\"b\\nc\" \"a\\\nbc\" bla\n" :: Text
     in do putStrLn $ T.unpack r
           print $ (t =~+ r :: [[Text]])

    let r = "([^\"]+)" :: Text
        t = "hel\"bla" :: Text
     in do putStrLn $ T.unpack r
           print $ (t =~+ r :: [[Text]])
    print $ showf  haskell
