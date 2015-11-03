{

{-# LANGUAGE OverloadedStrings			#-}
{-# LANGUAGE CPP				#-}
{-# OPTIONS_GHC -fno-warn-unused-binds		#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures	#-}
{-# OPTIONS_GHC -fno-warn-unused-matches	#-}
{-# OPTIONS_GHC -fno-warn-unused-imports	#-}
{-# OPTIONS_GHC -fno-warn-name-shadowing	#-}
{-# OPTIONS_GHC -fno-warn-tabs			#-}

module Gitomail.Maintainers.Lexer
  ( Alex(..)
  , AlexPosn(..)
  , AlexState(..)
  , Token(..)
  , TokenClass(..)
  , alexMonadScan
  , tokenToPosN
  , tokenToLineN
  , runAlex
  , alexStructError
  )
where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
#if ALEX_MAJOR < 3  || (ALEX_MAJOR == 3  &&  ALEX_MINOR <= 14)
import qualified Control.Monad as Control.Monad
#endif
}

%wrapper "monadUserState-bytestring"
$space = [ \ \t ]                           -- horizontal white space

$letter   = [a-zA-Z]
$digit    = 0-9
@sp       = $space*

state:-

  <0>         $space+                           ;
  <0,t,t2>    "#"[^\n]*                         ;
  <0,t,t2>    [\n]                              { begin 0 }
  <0>         alias                             { tok          TokenAlias }
  <0>         maintainer                        { tok          TokenMaintainer }
  <0>         reviewer                          { tok          TokenReviewer }
  <0>         observer                          { tok          TokenObserver }
  <0>         $letter [$letter $digit \_ \']*   { tok_string_i TokenIdentifier t }
  <t>         $space+                           { begin t2 }
  <t2>        [^\#\n]+                          { tok_string   TokenRestOfLine }

{

-- Some action helpers:
#if ALEX_MAJOR < 3  || (ALEX_MAJOR == 3  &&  ALEX_MINOR <= 14)
tok' r f (p, _, input) len = do
#else
tok' r f (p, _, input, _) len = do
#endif
   case r of
      Just i -> alexSetStartCode i
      Nothing -> return ()
   return $ Token p (f (B.take (fromIntegral len) input))

tok x = tok' Nothing (\s -> x)
tok_string' y x = tok' y (\s -> x (BS.pack $ B.unpack s))
tok_string x = tok_string' Nothing x
tok_string_i x i = tok_string' (Just i) x

alexStructError (line, column, e) = alexError $ "show-error: " ++ (show (line, column, e))
token_fail e ((AlexPn _ line column), _, input) len = alexStructError (line, column, e :: String)

-- The token type:
data Token = Token AlexPosn TokenClass
  deriving (Show)

tokenToPosN :: Token -> AlexPosn
tokenToPosN (Token p _) = p

tokenToLineN :: Token -> Int
tokenToLineN token = let (AlexPn _ line col) = tokenToPosN token in line

data TokenClass
 = TokenMaintainer
 | TokenReviewer
 | TokenObserver
 | TokenNewLine
 | TokenAlias
 | TokenEOF
 | TokenRestOfLine  BS.ByteString
 | TokenIdentifier  BS.ByteString
 deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = do
#if ALEX_MAJOR < 3  || (ALEX_MAJOR == 3  &&  ALEX_MINOR <= 1)
  (p, _, _) <- alexGetInput
#else
  (p, _, _, _) <- alexGetInput
#endif
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()

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
