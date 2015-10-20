{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}

module Gitomail.Gitomail
  ( MonadGitomail
  , ParameterNeeded(..)
  , compilePatterns
  , getCommitURL
  , getConfig
  , getDataFile
  , getExtraCCTo
  , getFooter
  , getFromEMail
  , getGitomail
  , getRefsMatcher
  , getRepoName
  , getRepositoryPath
  , getVersion
  , genExtraEMailHeaders
  , gitCmd
  , githashRepr
  , mapCommitHash
  , matchFiles
  , opts
  , withDB
  ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted    as E
import           Control.Lens.Operators      ((^.), (&))
import           Control.Monad               (forM,)
import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (StateT, evalStateT, gets, MonadState)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Char8       as BS8
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Maybe                  (fromMaybe)
import qualified Data.Map                    as Map
import           Data.Typeable               (Typeable)
import           Data.Version                (showVersion)
import           Database.LevelDB.Base       (DB)
import qualified Database.LevelDB.Base       as DB
import           Network.Mail.Mime           (Address (..))
import           System.Directory            (canonicalizePath,
                                              getCurrentDirectory, setCurrentDirectory)
import           System.FilePath             ((</>), takeBaseName)
import           System.Exit              (ExitCode (..))
----
import           Paths_gitomail              (version)
import qualified Gitomail.Config             as CFG
import qualified Gitomail.Maintainers        as Maintainers
import qualified Gitomail.Opts               as O
import qualified Gitomail.Version            as V
import           Lib.EMail                   (parseEMail', InvalidEMail(..))
import qualified Lib.Git                     as GIT
import qualified Lib.InlineFormatting        as F
import           Lib.Memo                    (cacheIO, cacheIO')
import           Lib.Process                 (readProcess, readProcess'')
import           Lib.Regex                   (matchWhole)
import qualified Paths_gitomail              as Paths_gitomail
------------------------------------------------------------------------------------

data Gitomail = Gitomail {
    opts               :: O.Opts
  , _loadFiles         :: (FilePath, O.GitRef) -> IO (GIT.Tree (Maybe BS8.ByteString))
  , _parseFiles        :: (FilePath, O.GitRef) -> IO (GIT.Tree (Maybe Maintainers.Unit))
  , _compilePatterns   :: (FilePath, O.GitRef) -> IO (GIT.Tree (Maybe [(Int, Maintainers.Definition)]))
  , _matchFiles        :: (FilePath, O.GitRef) -> IO (GIT.Tree Maintainers.AssignedFileStatus)
  , _getExtraCCTo      :: IO ([Address], [Address])
  , _getFromEMail      :: IO (Address)
  , _getRepositoryPath :: IO FilePath
  , _getConfig         :: IO CFG.Config
  , _withDB            :: forall a. forall m. (MonadMask m, MonadIO m) => (DB -> m a) -> m a
  }

data ParameterNeeded = ParameterNeeded String deriving (Typeable)
instance E.Exception ParameterNeeded
instance Show ParameterNeeded where
    show (ParameterNeeded msgstr) = "ParameterNeeded: " ++ msgstr

data GitRepoNotFound = GitRepoNotFound String deriving (Typeable)
instance E.Exception GitRepoNotFound
instance Show GitRepoNotFound where
    show (GitRepoNotFound msgstr) = "GitRepoNotFound: " ++ msgstr


getDataFile :: MonadIO m => FilePath -> m FilePath
getDataFile path = do
    liftIO $ Paths_gitomail.getDataFileName path

getGitomail :: O.Opts -> IO (Gitomail)
getGitomail opts = do
    -- Various cached loaders

    _loadFiles <- cacheIO $ \(filepath, gitref) ->
        Maintainers.loadFiles filepath gitref

    _parseFiles <- cacheIO $ \v -> do
        filesLoaded <- _loadFiles v
        Maintainers.parseFiles filesLoaded

    _compilePatterns <- cacheIO $ \v -> do
        filesParsed <- _parseFiles v
        Maintainers.compilePatterns filesParsed

    _matchFiles <- cacheIO $ \v -> do
        patternsCompiled <- _compilePatterns v
        Maintainers.matchFiles (Maintainers.assignDefinitionFiles patternsCompiled)

    _getRepositoryPath <- cacheIO' $ do
        -- On which repository are we working on? We need assistance from
        -- 'git' to find the '.git', and we use it either on O.repositoryPath
        -- or the current directory.

        origDir <- getCurrentDirectory
        maybe (return ()) setCurrentDirectory (opts ^. O.repositoryPath)
        (exitcode, stdout, stderr) <- readProcess'' "git" ["rev-parse", "--git-dir"] ""
        setCurrentDirectory origDir

        let path = fromMaybe origDir (opts ^. O.repositoryPath)
        case exitcode of
            ExitSuccess -> return $ path </> (T.unpack $ T.strip stdout)
            _ -> E.throw $ GitRepoNotFound $ (show (exitcode, stderr))

    _getConfig <-  cacheIO' $ do
        case opts ^. O.configPaths of
            []    -> (E.throw $ ParameterNeeded $ BS8.unpack "config paths")
            paths -> fmap CFG.final $ fmap (foldl1 CFG.combine) $ forM paths $ CFG.parse

    _getExtraCCTo <- cacheIO' $ do
        [cc, to] <- forM [(O.extraCC, "CC"), (O.extraTo, "To")] $ \(getter, name) -> do
            forM (map parseEMail' (opts ^. getter)) $ \case
                     Left r -> E.throw $ InvalidEMail $ name ++ ": " ++  r
                     Right r -> return r
        return (cc, to)

    _getFromEMail <- cacheIO' $ do
        config <- _getConfig
        case config ^. CFG.fromEMail of
            Nothing  -> E.throw $ ParameterNeeded $ BS8.unpack "from_email"
            Just fromEMail ->
                case parseEMail' fromEMail of
                    Left r -> E.throw $ InvalidEMail $ "from_email: " ++ r
                    Right r -> return r

    let _withDB f = do
            path <- liftIO $ _getRepositoryPath
            let gitomailDbPath = path </> "gitomail.db"
            let options = DB.defaultOptions { DB.createIfMissing = True , DB.errorIfExists = False }
            DB.withDB gitomailDbPath options f

    return $ Gitomail{..}

class (MonadIO m, MonadState Gitomail m, MonadBaseControl IO m, MonadMask m) => MonadGitomail m where
instance (MonadIO m, MonadBaseControl IO m, MonadMask m) => MonadGitomail (StateT Gitomail m) where

getRepositoryPath :: (MonadGitomail m) => m FilePath
getRepositoryPath = gets _getRepositoryPath >>= liftIO

getConfig         :: (MonadGitomail m) => m CFG.Config
getConfig         = gets _getConfig >>= liftIO

getFromEMail      :: (MonadGitomail m) => m Address
getFromEMail      = gets _getFromEMail >>= liftIO

getExtraCCTo      :: (MonadGitomail m) => m ([Address], [Address])
getExtraCCTo      = gets _getExtraCCTo >>= liftIO

matchFiles        :: (MonadGitomail m) =>
                    (FilePath, O.GitRef) -> m (GIT.Tree Maintainers.AssignedFileStatus)
matchFiles x      = gets _matchFiles >>= \f -> liftIO $ f x

compilePatterns   :: (MonadGitomail m) =>
                     (FilePath, O.GitRef) -> m (GIT.Tree (Maybe [(Int, Maintainers.Definition)]))
compilePatterns x = gets _compilePatterns >>= \f -> liftIO $ f x

getRepoName :: (MonadGitomail m) => m Text
getRepoName = do
    config <- getConfig
    opts <- gets opts
    case (config ^. CFG.repoName, opts ^. O.repositoryPath) of
        (Just name, _) ->
            -- Prefer taking the repoName from the config file.
            return name
        (Nothing, Just repoPath) -> do
            -- If the user-provided repoPath is a working directory that
            -- has .git, we can guess the repo name from its name.
            canon <- liftIO $ canonicalizePath repoPath
            return $ T.pack $ takeBaseName canon
        (Nothing, Nothing) -> return "?"

getCommitURL :: (MonadGitomail m) => Text -> m (Maybe Text)
getCommitURL hash = do
    config <- getConfig
    repoName <- getRepoName
    case config ^. CFG.commitURL of
        Nothing -> return Nothing
        Just t -> return $ Just $ t
                     & T.replace "%r" repoName
                     & T.replace "%H" hash

withDB :: (MonadGitomail m) => StateT DB m a -> m a
withDB = (\x -> gets _withDB >>= \f -> f x) . evalStateT

gitCmd :: (MonadGitomail m) => [Text] -> m Text
gitCmd params = do
    repoPath <- getRepositoryPath
    readProcess "git" (["--git-dir", T.pack repoPath] ++ params)

getRefsMatcher :: (MonadGitomail m) => m (Text -> Bool)
getRefsMatcher = do
    config <- getConfig
    let regexMatcher getter f =
            case config ^. getter of
                Just regexes -> \ref -> f $ or $ map (flip matchWhole ref) regexes
                Nothing -> const True

    let excludeRefs = regexMatcher CFG.excludeRefs not
    let includeRefs = regexMatcher CFG.includeRefs id
    let onlyRefs x = and [excludeRefs x, includeRefs x]
    return onlyRefs

githashRepr :: (MonadGitomail m) => GIT.GitCommitHash -> m Text
githashRepr githash = do
    config <- getConfig
    return $ T.take (config CFG.^.|| CFG.hashSize) githash

getVersion :: Text
getVersion = T.concat [T.pack $ showVersion version, ", git ", V.version]

getFooter :: (MonadGitomail m) => m F.FList
getFooter = do
    config <- getConfig

    let v = [(getVersion, [F.Footer])]
        v' = maybe v (const []) $ config ^. CFG.hashMap

    return $ [ (T.concat ["Sent by "], [F.Footer])
             , (T.concat ["Gitomail "], [F.Footer, F.Link "http://github.com/kernelim/gitomail", F.Dark])
             , (T.concat [" "], [F.Footer])] ++ v'

mapCommitHash :: (MonadGitomail m) => GIT.GitCommitHash -> m (GIT.GitCommitHash)
mapCommitHash h = do
    config <- getConfig
    return $  maybe h (\m -> fromMaybe h (Map.lookup h m)) (config ^. CFG.hashMap)

genExtraEMailHeaders :: (MonadGitomail m) => Address -> m [(BS8.ByteString, Text)]
genExtraEMailHeaders (Address _ _) = do
    config <- getConfig

    let v = case config ^. CFG.hashMap of
               Nothing -> T.concat [T.pack $ showVersion version, " ", V.version]
               Just _ -> "0.0.0"
    return [("X-Mailer", T.concat ["gitomail ", v])]

