{
module Gitomail.Maintainers.Parser where

import Gitomail.Maintainers.Base
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M

}

%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        "alias"           { Token _ TokenAlias           }
        "maintainer"      { Token _ TokenMaintainer      }
        "reviewer"        { Token _ TokenReviewer        }
        "observer"        { Token _ TokenObserver        }
        REST_OF_LINE      { Token _ (TokenRestOfLine $$) }
        ID                { Token _ (TokenIdentifier $$) }

%%

-- Regular types

Root  :: {Unit}
       : UnitDefs { Unit $1 }

UnitDefs :: {[(Int, UnitDef)]}
       :                          { [] }
       | UnitDef UnitDefs         { $1:$2 }

UnitDef :: {(Int, UnitDef)}
       : "maintainer" ID RestOfLine    { (tokenToLineN $1, Maintainer $2 $3) }
       | "reviewer"   ID RestOfLine    { (tokenToLineN $1, Reviewer   $2 $3) }
       | "observer"   ID RestOfLine    { (tokenToLineN $1, Observer   $2 $3) }
       | "alias"      ID REST_OF_LINE  { (tokenToLineN $1, Alias      $2 $3) }

RestOfLine :: {Maybe BS8.ByteString}
       : REST_OF_LINE                  { Just $1 }
       |                               { Nothing }

{}
