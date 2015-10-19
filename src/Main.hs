{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Main (main) where

------------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy        as BL
import qualified Data.Text                   as T
import           Control.Lens.Operators      ((^.))
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.State.Strict  (evalStateT, gets)
import           Options.Applicative         (execParser)
import           System.Exit                 (exitFailure)

----
import qualified Gitomail.Config             as CFG
import qualified Gitomail.Maintainers        as Maintainers
import qualified Gitomail.Opts               as O
import           Gitomail.Gitomail
import           Gitomail.CommitToMail
import           Gitomail.WhoMaintains
import           Gitomail.Automailer
import           Lib.LiftedPrelude
------------------------------------------------------------------------------------

runCmd :: (MonadGitomail m) => m ()
runCmd = do
    opts' <- gets opts
    if opts' ^. O.version
        then printVersion
        else action $ opts' ^. O.runCommand

    where
        printVersion = do
            putStrLn $ "Gitolite " ++ T.unpack getVersion

        action Nothing = do
            putStrLn "No action given (run with --help)"
            liftIO $ exitFailure

        action (Just x) = case x of
            -- Commands
            O.WhoMaintainsCmnd             -> whoMaintains
            O.ShowIneffectiveDefinitions   -> showEffectiveDefs
            O.SendOne                      -> sendOne
            O.AutoMailer                   -> autoMailer

            -- Debug
            O.CheckBranchPoints            -> checkBranchPoints
            O.ForgetHash                   -> forgetHash
            O.ShowAutoMailerRefs           -> showAutoMailerRefs
            O.ParseMaintainerFile filepath -> do
                content <- liftIO $ BL.readFile filepath
                print (Maintainers.parse content)
            O.ParseConfigFile filepath     -> do
                config <- CFG.parse filepath
                print config
            O.EvalConfigs -> do
                config <- getConfig
                print config

main :: IO ()
main = void $ execParser O.opts >>= getGitomail >>= evalStateT runCmd