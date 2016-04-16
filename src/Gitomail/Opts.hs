{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Gitomail.Opts
  ( Opts(..)
  , Command(..)
  , GitRef
  , opts
  , runCommand
  , configPaths
  , noImplicitConfigs
  , repositoryPath
  , repositoryName
  , outputPath
  , version
  , verbose
  , extraCC
  , extraTo
  , dryRun
  , gitRef
  ) where

------------------------------------------------------------------------------------
import           Control.Lens        (makeLenses)
import           Data.Text           (Text)
import qualified Data.Text           as T

import           Options.Applicative (many, short, long, Mod, OptionFields,
                                      Parser, strOption, switch, help, (<>),
                                      optional, subparser, command, info,
                                      argument, str, metavar, progDesc,
                                      ParserInfo, helper, idm, (<**>))
------------------------------------------------------------------------------------

type RepPath = FilePath
type GitRef = Text
type EMailAddress = Text
type DryRun = Bool

data Command
    = WhoMaintainsCmnd
    | ShowIneffectiveDefinitions
    | SendOne
    | ShowOne
    | AutoMailer
    | ShowAutoMailerRefs
    | ForgetHash
    | ParseMaintainerFile FilePath
    | ParseConfigFile FilePath
    | JiraCCByIssue String
    | EvalConfigs
    | Misc
    deriving (Show)

textOption :: Mod OptionFields String -> Parser Text
textOption = (fmap T.pack) . strOption

data Opts = Opts
    { _verbose             :: Bool
    , _version             :: Bool
    , _dryRun              :: DryRun
    , _outputPath          :: Maybe FilePath
    , _configPaths         :: [FilePath]
    , _noImplicitConfigs   :: Bool
    , _extraCC             :: [EMailAddress]
    , _extraTo             :: [EMailAddress]
    , _repositoryPath      :: Maybe RepPath
    , _repositoryName      :: Maybe Text
    , _gitRef              :: Maybe GitRef
    , _runCommand          :: Maybe Command
    } deriving (Show)

makeLenses ''Opts

optsParse :: Parser Opts
optsParse = Opts
     <$> switch ( long "debug"     <> short 'd'  <> help "Enable debug prints" )
     <*> switch ( long "version"   <> short 'v'  <> help "Just print version and exit" )
     <*> switch ( long "dry-run"   <> short 'n'  <> help "Don't actually send or output E-Mails, nor update DB" )
     <*> ( optional . strOption)
         ( long "output-path"      <> short 'o'  <> help "Local directory in which to place E-Mails instead of sending by SMTP" )
     <*> ( many . strOption)
         ( long "config"           <> short 'c'  <> help "Configuration files" )
     <*> switch ( long "no-implicit-configs"     <> help "Don't read configs paths such as ~/.gitomailconf.yaml or $GIT_DIR/gitomailconf.yaml" )
     <*> ( many . textOption)      ( long "cc"    <> help "Extra people for 'Cc:' in this invocation" )
     <*> ( many . textOption)      ( long "to"    <> help "Extra people for 'To:' in this invocation" )
     <*> ( optional . strOption)
                ( long "repo"      <> short 'r'   <> help "Repository pathname" )
     <*> ( optional . textOption)
                ( long "repo-name" <> help "Repository name" )
     <*> ( optional . textOption)
                ( long "g"         <> short 'g'   <> help "Git revision" )
     <*> optional (subparser
           (
              command "who-maintains" showWhoMaintains
           <> command "show-ineffectives" showIneffectiveDefinitions
           <> command "show-one" showOneRef
           <> command "send-one" sendOneRef
           <> command "auto-mailer" autoMailer
           <> command "debug" debugCommands
           ))
    where
        oneArg ctr = info (ctr <$> (argument str (metavar "ARG")))

        debugCommands = info (subparser (
                  command "parse-maintainers-file" (oneArg     ParseMaintainerFile (progDesc ""))
               <> command "parse-config-file"      (oneArg     ParseConfigFile     (progDesc ""))
               <> command "eval-configs"           (info (pure EvalConfigs)        (progDesc ""))
               <> command "show-auto-mailer-refs"  (info (pure ShowAutoMailerRefs) (progDesc ""))
               <> command "forget-hash"            (info (pure ForgetHash)         (progDesc ""))
               <> command "jira-cc-by-issue"       (oneArg     JiraCCByIssue       (progDesc ""))
               <> command "misc"                   (info (pure Misc)               (progDesc ""))
            )) (progDesc "Various debugging commands")

        showWhoMaintains = info (pure WhoMaintainsCmnd)
            (progDesc "Show current state of maintainership")

        showIneffectiveDefinitions = info (pure ShowIneffectiveDefinitions)
            (progDesc "Show ineffective statements in the tree")

        sendOneRef = info (pure SendOne)
            (progDesc "Send a single commit E-Mail for a specified git revision")

        showOneRef = info (pure ShowOne)
            (progDesc "Console print of a single commit, in ANSI")

        autoMailer = info (pure AutoMailer)
            (progDesc "Automatically send mail for new commits (read the docs first!)")

opts :: ParserInfo Opts
opts = info (optsParse <**> helper) idm
