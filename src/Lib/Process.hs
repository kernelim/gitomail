{-# LANGUAGE OverloadedStrings #-}

module Lib.Process (
    readProcess,
    readProcess',
    readProcess'',
    ReadProcessFailed
    ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted as E
import           Control.Monad.IO.Class   (MonadIO, liftIO)
import           Data.Text                (Text, unpack)
import           Data.Typeable            (Typeable)
import           System.Exit              (ExitCode (..))
import qualified System.Process.Text      as SP
------------------------------------------------------------------------------------

data ReadProcessFailed = ReadProcessFailed String deriving (Typeable)
instance E.Exception ReadProcessFailed
instance Show ReadProcessFailed where
  show (ReadProcessFailed msgstr) = "ReadProcessFailed: " ++ msgstr

readProcess :: MonadIO m => FilePath -> [Text] -> m Text
readProcess fp params = readProcess' fp params ""

readProcess' :: MonadIO m => FilePath -> [Text] -> Text -> m Text
readProcess' fp params input = do
    (exitcode, stdout, stderr) <-
        liftIO $ SP.readProcessWithExitCode fp (map unpack params) input
    case exitcode of
        ExitSuccess -> return stdout
        _ -> E.throw $ ReadProcessFailed $ (show (fp, params, exitcode, stderr))

readProcess'' :: MonadIO m => FilePath -> [Text] -> Text -> m (ExitCode, Text, Text)
readProcess'' fp params input = do
    liftIO $ SP.readProcessWithExitCode fp (map unpack params) input
