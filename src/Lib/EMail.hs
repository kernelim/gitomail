{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib.EMail (emailRegEx, parseEMail, parseEMail', InvalidEMail(..)) where

------------------------------------------------------------------------------------
import           Network.Mail.Mime           (Address (..))
import           Data.Text                   (Text)
import qualified Data.Text                  as T
import qualified Control.Exception.Lifted   as E
import           Data.Typeable               (Typeable)
import           Text.Regex.TDFA             ((=~))
import           Text.Regex.TDFA.Text        ()
------------------------------------------------------------------------------------

data InvalidEMail = InvalidEMail String deriving (Typeable)
instance E.Exception InvalidEMail
instance Show InvalidEMail where
    show (InvalidEMail msgstr) = "InvalidEMail: " ++ msgstr

emailRegEx :: Text
emailRegEx = "((([^\n]*) )<([^@\n]+@[^\n>]+)>|([^@\n]+@[^\n>]+))"

parseEMail' :: Text -> Either String Address
parseEMail' e = do
    let parse = map d (e  =~ emailRegEx :: [[Text]])
        d [_ , _, _, name, email, ""] = Right $ Address (Just name) email
        d [_ , _, _, "", "", email]   = Right $ Address Nothing email
        d r@_                         = Left r

    case parse of
        [Right r] -> Right r
        [Left r]  -> Left $ "regex result was " ++ show r
        []        -> Left "no regex matches"
        _         -> Left "too many regex matches"

parseEMail :: Monad m => Text -> m Address
parseEMail e = do
    case parseEMail' e of
        Right r -> return r
        Left r  -> E.throw $ InvalidEMail $ T.unpack e ++ ", " ++ show r
