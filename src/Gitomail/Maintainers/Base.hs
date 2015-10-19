{-# LANGUAGE OverloadedStrings         #-}

module Gitomail.Maintainers.Base
  ( thenP
  , returnP
  , happyError
  , Parser
  , Token(..)
  , TokenClass(..)
  , AlexState(..)
  , Unit(..)
  , UnitDef(..)
  , AliasName
  , EMail
  , lexer
  , module Gitomail.Maintainers.Lexer
  )
  where

import           Gitomail.Maintainers.Lexer
import qualified Data.ByteString         as BS

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

alexGetPosition :: Alex (AlexPosn)
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

happyError :: Parser a
happyError = do
  (AlexPn _ line col) <- alexGetPosition
  alexStructError (line, col, "syntax error" :: String)

-- Now we define the data types of the Syntax

type Scope = BS.ByteString
type AliasName = BS.ByteString
type EMail = BS.ByteString
type LineNum = Int

data UnitDef
  = Alias AliasName EMail
  | Observer AliasName (Maybe Scope)
  | Maintainer AliasName (Maybe Scope)
  | Reviewer AliasName (Maybe Scope)
  deriving (Eq, Show)

data Unit
  = Unit {
      unitDefs :: [(LineNum, UnitDef)]
    } deriving (Eq, Show)

-- Link the lexer and the parser:
lexer :: (Token -> Parser a) -> Parser a
lexer f = alexMonadScan >>= f
