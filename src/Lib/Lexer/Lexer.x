{

{-# LANGUAGE OverloadedStrings			#-}
{-# LANGUAGE NoMonomorphismRestriction	        #-}
{-# LANGUAGE CPP				#-}
{-# OPTIONS_GHC -fno-warn-unused-binds		#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures	#-}
{-# OPTIONS_GHC -fno-warn-unused-matches	#-}
{-# OPTIONS_GHC -fno-warn-unused-imports	#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing	#-}
{-# OPTIONS_GHC -fno-warn-tabs			#-}
{-# OPTIONS_GHC -funbox-strict-fields           #-}

module Lib.Lexer.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexMonadScan
  , alexSetStartCode
  , alexStructError
  , runAlex
  , clang
  , haskell
  )
where

import Data.Int (Int64)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Lib.SourceHighlight.Data (Element(..))

#if ALEX_MAJOR < 3  || (ALEX_MAJOR == 3  &&  ALEX_MINOR <= 14)
import qualified Control.Monad as Control.Monad
#endif
}

%wrapper "monadUserState-bytestring"

$space = [ \ \t ]
$eol   = \n

$letter		= [a-zA-Z]
$digit		= 0-9
$digitzero    	= 0-9
$digit  	= 1-9
$digitoct	= 0-7
$hexdigit	= [0-9a-fA-F]

$cidentletter	= [a-zA-Z_\$]

$validstr       = [^ \\ \" \n ]
$validchar      = [^ \\ \' \n ]
$nonwhitspace   = . # $white

@sp             = $white+
@punct          = [ \^ \~
   		\[ \] \( \) \- \+
		\{ \} \\ \: \?
		\! \@ \# \$ \% \;
		\&    \, \/ \| \.
                \` \" \< \> \* \= ]
@cId            = [ a-z A-Z 0-9 \_ \$ ]+
@haskTypeConst  = [A-Z_][a-z A-Z 0-9 \_ ']*
@haskBind       = [a-z_][a-z A-Z 0-9 \_ ']*

state:-

  <0>         @sp                     { tok       Ignore  	    }

  <clang>     @sp                     { tok       Ignore  	    }
  <clang>     [\"]                    { tokPush   String    str     }
  <str>       "\\\""                  { tok       String            }
  <str>       [\"]                    { tokPop    String            }
  <str>       [^ \\ \"]+              { tok       String            }
  <str>       [\\]                    { tok       String            }

  <clang>     [\']                    { tokPush   Char      charx   }
  <charx>     '\\\"'                  { tok       Char              }
  <charx>     [\']                    { tokPop    Char              }
  <charx>     [\n]                    { tokPop    Char              }
  <charx>     [^ \\ \']+              { tok       Char              }
  <charx>     [\\]                    { tok       Char              }

  <clang>     "/*"                    { tokPush   Comment   ccomm   }
  <ccomm>      "*/"                   { tokPop    Comment           }
  <ccomm>      [ [^ \*] \n]+          { tok       Comment           }
  <ccomm>      [\*]                   { tok       Comment           }
  <clang>     "//"                    { tokPush   Comment   comm2   }
  <comm2>     [ ^ \n ]*\n             { tokPop    Comment           }

  <clang>     "_Pragma"               { tok       Keyword           }
  <clang>     "__attribute__"         { tok       Keyword           }
  <clang>     "asm"                   { tok       Keyword           }
  <clang>     "auto"                  { tok       Keyword           }
  <clang>     "break"                 { tok       Keyword           }
  <clang>     "case"                  { tok       Keyword           }
  <clang>     "char"                  { tok       Keyword           }
  <clang>     "const"                 { tok       Keyword           }
  <clang>     "continue"              { tok       Keyword           }
  <clang>     "default"               { tok       Keyword           }
  <clang>     "define"                { tok       Keyword           }
  <clang>     "do"                    { tok       Keyword           }
  <clang>     "double"                { tok       Keyword           }
  <clang>     "else"                  { tok       Keyword           }
  <clang>     "endif"                 { tok       Keyword           }
  <clang>     "enum"                  { tok       Keyword           }
  <clang>     "extern"                { tok       Keyword           }
  <clang>     "float"                 { tok       Keyword           }
  <clang>     "for"                   { tok       Keyword           }
  <clang>     "goto"                  { tok       Keyword           }
  <clang>     "if"                    { tok       Keyword           }
  <clang>     "ifdef"                 { tok       Keyword           }
  <clang>     "ifndef"                { tok       Keyword           }
  <clang>     "include"               { tok       Keyword           }
  <clang>     "include_once"          { tok       Keyword           }
  <clang>     "inline"                { tok       Keyword           }
  <clang>     "int"                   { tok       Keyword           }
  <clang>     "int"                   { tok       Keyword           }
  <clang>     "long"                  { tok       Keyword           }
  <clang>     "pragma"                { tok       Keyword           }
  <clang>     "register"              { tok       Keyword           }
  <clang>     "return"                { tok       Keyword           }
  <clang>     "short"                 { tok       Keyword           }
  <clang>     "signed"                { tok       Keyword           }
  <clang>     "sizeof"                { tok       Keyword           }
  <clang>     "static"                { tok       Keyword           }
  <clang>     "struct"                { tok       Keyword           }
  <clang>     "switch"                { tok       Keyword           }
  <clang>     "typedef"               { tok       Keyword           }
  <clang>     "undef"                 { tok       Keyword           }
  <clang>     "union"                 { tok       Keyword           }
  <clang>     "unsigned"              { tok       Keyword           }
  <clang>     "void"                  { tok       Keyword           }
  <clang>     "volatile"              { tok       Keyword           }
  <clang>     "while"                 { tok       Keyword           }

  <clang>     [ 0-9               ]+  { tok       Number            }
  <clang>     @cId                    { tok       Identifier        }
  <clang>     @punct                  { tok       Ignore            }
  <clang>     .                       { tok       Ignore            }

  <haskell>   @sp                     { tok       Ignore  	    }
  <haskell>   [\"]                    { tokPush   String    str     }
  <haskell>   [\']                    { tokPush   Char      charx   }
  <haskell>   "--"                    { tokPush   Comment   comm2   }

  <haskell>   "_"                     { tok       Ignore            }

  <haskell>   "let"                   { tok       Keyword           }
  <haskell>   "as"                    { tok       Keyword           }
  <haskell>   "case"                  { tok       Keyword           }
  <haskell>   "of"                    { tok       Keyword           }
  <haskell>   "class"                 { tok       Keyword           }
  <haskell>   "data"                  { tok       Keyword           }
  <haskell>   "data family"           { tok       Keyword           }
  <haskell>   "data instance"         { tok       Keyword           }
  <haskell>   "default"               { tok       Keyword           }
  <haskell>   "deriving"              { tok       Keyword           }
  <haskell>   "deriving instance"     { tok       Keyword           }
  <haskell>   "do"                    { tok       Keyword           }
  <haskell>   "forall"                { tok       Keyword           }
  <haskell>   "foreign"               { tok       Keyword           }
  <haskell>   "hiding"                { tok       Keyword           }
  <haskell>   "if"                    { tok       Keyword           }
  <haskell>   "import"                { tok       Keyword           }
  <haskell>   "then"                  { tok       Keyword           }
  <haskell>   "else"                  { tok       Keyword           }
  <haskell>   "infix"                 { tok       Keyword           }
  <haskell>   "infixl"                { tok       Keyword           }
  <haskell>   "infixr"                { tok       Keyword           }
  <haskell>   "instance"              { tok       Keyword           }
  <haskell>   "let"                   { tok       Keyword           }
  <haskell>   "in"                    { tok       Keyword           }
  <haskell>   "mdo"                   { tok       Keyword           }
  <haskell>   "module"                { tok       Keyword           }
  <haskell>   "newtype"               { tok       Keyword           }
  <haskell>   "proc"                  { tok       Keyword           }
  <haskell>   "qualified"             { tok       Keyword           }
  <haskell>   "rec"                   { tok       Keyword           }
  <haskell>   "type"                  { tok       Keyword           }
  <haskell>   "type family"           { tok       Keyword           }
  <haskell>   "type instance"         { tok       Keyword           }
  <haskell>   "where"                 { tok       Keyword           }

  <haskell>    [ 0-9              ]+  { tok       Number            }

  <haskell>    @haskTypeConst         { tok       Type              }
  <haskell>    @haskBind              { tok       Ignore            }

  <haskell>    @punct                 { tok       Ignore            }
  <haskell>    .                      { tok       Ignore            }

{

-- Some action helpers:

#if ALEX_MAJOR < 3  || (ALEX_MAJOR == 3  &&  ALEX_MINOR <= 14)
tok x (p, _, input)    len = do
#else
tok x (p, _, input, _) len = do
#endif
    return $ Token (TokenDemark x) (B.take (fromIntegral len) input)

tokPush cls code inp len = do
    alexGetStartCode >>= pushStateStack
    alexSetStartCode code >> tok cls inp len
    where pushStateStack :: Int -> Alex ()
          pushStateStack state = Alex $ \s@AlexState{alex_ust=ust} -> Right (s {alex_ust = state:ust}, ())

tokPop  cls inp len = do
    popStateStack >>= alexSetStartCode >> tok cls inp len
    where popStateStack :: Alex Int
          popStateStack = Alex $ \s@AlexState{alex_ust=ust} -> Right (s {alex_ust = tail ust}, head ust)

alexStructError (line, column, e) = alexError $ "show-error: " ++ (show (line, column, e))
token_fail e ((AlexPn _ line column), _, input) len = alexStructError (line, column, e :: String)

-- The token type:
data Token = Token TokenClass B.ByteString
  deriving (Show)

data TokenClass
 = TokenDemark !Element
 | TokenEOF
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
#if ALEX_MAJOR < 3  || (ALEX_MAJOR == 3  &&  ALEX_MINOR <= 1)
  (p, _, _) <- alexGetInput
#else
  (p, _, _, _) <- alexGetInput
#endif
  return $ Token TokenEOF ""

type AlexUserState = [Int]
alexInitUserState = []

#if ALEX_MAJOR < 3  || (ALEX_MAJOR == 3  &&  ALEX_MINOR <= 1)

instance Functor Alex where
  fmap f m = do x <- m; return (f x)

instance Applicative Alex where
  pure = return
  (<*>) = Control.Monad.ap

#endif

#if ALEX_MAJOR == 3  &&  ALEX_MINOR < 1

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes i = i   -- no pending bytes when lexing bytestrings

#endif

}