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
tok' r f (p, _, input, _) len = do
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
  (p, _, _, _) <- alexGetInput
  return $ Token p TokenEOF

type AlexUserState = ()
alexInitUserState = ()
}
