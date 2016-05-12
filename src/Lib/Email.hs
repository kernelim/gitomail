{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Email (emailRegEx, parseEmail, parseEmail', InvalidEmail(..)) where

------------------------------------------------------------------------------------
import           Network.Mail.Mime           (Address (..))
import           Data.Text                   (Text)
import qualified Data.Text                  as T
import qualified Control.Exception.Lifted   as E
import           Data.Typeable               (Typeable)
import           Text.Regex.TDFA             ((=~))
import           Text.Regex.TDFA.Text        ()
------------------------------------------------------------------------------------

data InvalidEmail = InvalidEmail String deriving (Typeable)
instance E.Exception InvalidEmail
instance Show InvalidEmail where
    show (InvalidEmail msgstr) = "InvalidEmail: " ++ msgstr

emailRegEx :: Text
emailRegEx = "((([^\n]*) )<([^@\n]+@[^\n>]+)>|([^@\n]+@[^\n>]+))"

parseEmail' :: Text -> Either String Address
parseEmail' e = do
    let parse = map d (e  =~ emailRegEx :: [[Text]])
        d [_ , _, _, name, email, ""] = Right $ Address (Just name) email
        d [_ , _, _, "", "", email]   = Right $ Address Nothing email
        d r@_                         = Left r

    case parse of
        [Right r] -> Right r
        [Left r]  -> Left $ "regex result was " ++ show r
        []        -> Left "no regex matches"
        _         -> Left "too many regex matches"

parseEmail :: Monad m => Text -> m Address
parseEmail e = do
    case parseEmail' e of
        Right r -> return r
        Left r  -> E.throw $ InvalidEmail $ T.unpack e ++ ", " ++ show r

deriving instance Ord Address
