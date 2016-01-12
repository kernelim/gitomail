{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Gitomail.Gitomail
  ( MonadGitomail
  , ParameterNeeded(..)
  , compilePatterns
  , getCommitURL
  , getBlobInCommitURL
  , getConfig
  , getExtraCCTo
  , getFooter
  , getFromEMail
  , getGitomail
  , getRefsMatcher
  , getRepoName
  , getRepositoryPath
  , getVersion
  , getTopAliases
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
import           Control.Lens.Operators      ((^.), (&), (<+=))
import           Control.Lens                (makeLenses)
import           Control.Monad               (forM,)
import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (StateT, gets, MonadState)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Char8       as BS8
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Maybe                  (fromMaybe, catMaybes)
import qualified Data.Map                    as Map
import           Data.Typeable               (Typeable)
import           Data.Version                (showVersion)
import           Database.LevelDB.Base       (DB)
import qualified Database.LevelDB.Base       as DB
import           Network.Mail.Mime           (Address (..))
import           Data.UnixTime               (getUnixTime, UnixTime(..))
import           System.Directory            (canonicalizePath, doesFileExist,
                                              getCurrentDirectory, setCurrentDirectory)
import           System.FilePath             ((</>), takeBaseName)
import           System.Exit                 (ExitCode (..))
import           System.Environment          (getEnv)
import           System.Random               as Rand
import           Data.Char                   (chr, ord)
----
import           Paths_gitomail              (version)
import qualified Gitomail.Config             as CFG
import qualified Gitomail.Maintainers        as Maintainers
import qualified Gitomail.Opts               as O
import qualified Gitomail.Version            as V
import           Lib.EMail                   (parseEMail', InvalidEMail(..))
import qualified Lib.Git                     as GIT
import qualified Lib.Formatting              as F
import           Lib.Text                    ((+@), showT, leadingZeros,
                                              safeDecode)
import           Lib.Memo                    (cacheIO, cacheIO')
import           Lib.Process                 (readProcess, readProcess'')
import           Lib.Regex                   (matchWhole)
------------------------------------------------------------------------------------

data Gitomail = Gitomail {
    opts                :: O.Opts
  , _fakeMessageId      :: Int
  , __loadFiles         :: (FilePath, O.GitRef) -> IO (GIT.Tree (Maybe BS8.ByteString))
  , __parseFiles        :: (FilePath, O.GitRef) -> IO (GIT.Tree (Maybe Maintainers.Unit))
  , __compilePatterns   :: (FilePath, O.GitRef) -> IO (GIT.Tree (Maybe [Maintainers.DefInFile]))
  , __matchFiles        :: (FilePath, O.GitRef) -> IO (GIT.Tree Maintainers.AssignedFileStatus)
  , __getExtraCCTo      :: IO ([Address], [Address])
  , __getFromEMail      :: IO (Address)
  , __getRepositoryPath :: IO FilePath
  , __getConfig         :: IO CFG.Config
  }

makeLenses ''Gitomail

data ParameterNeeded = ParameterNeeded String deriving (Typeable)
instance E.Exception ParameterNeeded
instance Show ParameterNeeded where
    show (ParameterNeeded msgstr) = "ParameterNeeded: " ++ msgstr

data GitRepoNotFound = GitRepoNotFound String deriving (Typeable)
instance E.Exception GitRepoNotFound
instance Show GitRepoNotFound where
    show (GitRepoNotFound msgstr) = "GitRepoNotFound: " ++ msgstr


getGitomail :: O.Opts -> IO (Gitomail)
getGitomail opts = do
    -- Various cached loaders

    let _fakeMessageId = 0

    __loadFiles <- cacheIO $ \(filepath, gitref) ->
        Maintainers.loadFiles filepath gitref

    __parseFiles <- cacheIO $ \v -> do
        filesLoaded <- __loadFiles v
        Maintainers.parseFiles filesLoaded

    __compilePatterns <- cacheIO $ \v -> do
        filesParsed <- __parseFiles v
        Maintainers.compilePatterns filesParsed

    __matchFiles <- cacheIO $ \v -> do
        patternsCompiled <- __compilePatterns v
        Maintainers.matchFiles (Maintainers.assignDefinitionFiles patternsCompiled)

    __getRepositoryPath <- cacheIO' $ do
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

    __getConfig <-  cacheIO' $ do
        let safeGetEnv v =
                E.catch (fmap Just $ getEnv v) (\(_ :: E.SomeException) -> return Nothing)
            existence e = fmap (\case True -> [e] ; False -> []) $ doesFileExist e
            homeConfig' =
                safeGetEnv "HOME" >>= \case
                    Nothing -> return []
                    Just homePath -> existence $ homePath </> ".gitomailconf.yaml"
            repoConfig' = do
                repoPath <- __getRepositoryPath
                existence $ repoPath </> "gitomailconf.yaml"
            checkOpt a = if opts ^. O.noImplicitConfigs then return [] else a

        homeConfig <- checkOpt homeConfig'
        repoConfig <- checkOpt repoConfig'

        case homeConfig ++ repoConfig ++ opts ^. O.configPaths of
            []    -> (E.throw $ ParameterNeeded $ BS8.unpack "config paths")
            paths -> fmap CFG.final $ fmap (foldl1 CFG.combine) $ forM paths $ CFG.parse

    __getExtraCCTo <- cacheIO' $ do
        [cc, to] <- forM [(O.extraCC, "CC"), (O.extraTo, "To")] $ \(getter, name) -> do
            forM (map parseEMail' (opts ^. getter)) $ \case
                     Left r -> E.throw $ InvalidEMail $ name ++ ": " ++  r
                     Right r -> return r
        return (cc, to)

    __getFromEMail <- cacheIO' $ do
        config <- __getConfig
        case config ^. CFG.fromEMail of
            Nothing  -> E.throw $ ParameterNeeded $ BS8.unpack "from_email"
            Just fromEMail ->
                case parseEMail' fromEMail of
                    Left r -> E.throw $ InvalidEMail $ "from_email: " ++ r
                    Right r -> return r

    return $ Gitomail{..}

class (MonadIO m, MonadState Gitomail m, MonadBaseControl IO m, MonadMask m) => MonadGitomail m where
instance (MonadIO m, MonadBaseControl IO m, MonadMask m) => MonadGitomail (StateT Gitomail m) where

getRepositoryPath :: (MonadGitomail m) => m FilePath
getRepositoryPath = gets __getRepositoryPath >>= liftIO

getConfig         :: (MonadGitomail m) => m CFG.Config
getConfig         = gets __getConfig >>= liftIO

getFromEMail      :: (MonadGitomail m) => m Address
getFromEMail      = gets __getFromEMail >>= liftIO

getExtraCCTo      :: (MonadGitomail m) => m ([Address], [Address])
getExtraCCTo      = gets __getExtraCCTo >>= liftIO

matchFiles        :: (MonadGitomail m) =>
                    (FilePath, O.GitRef) -> m (GIT.Tree Maintainers.AssignedFileStatus)
matchFiles x      = gets __matchFiles >>= \f -> liftIO $ f x

compilePatterns   :: (MonadGitomail m) =>
                     (FilePath, O.GitRef) -> m (GIT.Tree (Maybe [(Int, Maintainers.Definition)]))
compilePatterns x = gets __compilePatterns >>= \f -> liftIO $ f x

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

getBlobInCommitURL :: (MonadGitomail m) => m (Maybe (Text -> Text -> Text))
getBlobInCommitURL = do
    config <- getConfig
    repoName <- getRepoName
    case config ^. CFG.blobInCommitURL of
        Nothing -> return Nothing
        Just t -> return $ Just $ (\hash filename -> t
                     & T.replace "%r" repoName
                     & T.replace "%H" hash
                     & T.replace "%f" filename)

withDB :: (MonadGitomail m) => (DB -> m a) -> m a
withDB f = do
    path <- getRepositoryPath
    let gitomailDbPath = path </> "gitomail.db"
    let options = DB.defaultOptions { DB.createIfMissing = True , DB.errorIfExists = False }
    DB.withDB gitomailDbPath options f

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

getTopAliases :: (MonadGitomail m) => O.GitRef -> m (Map.Map Text Address)
getTopAliases gitRef = do
    repoPath <- getRepositoryPath
    patternsCompiled <- compilePatterns (repoPath, gitRef)
    let f (Maintainers.Alias name email) = do
            case parseEMail' $ safeDecode email of
                Left _        -> return Nothing
                Right address -> return $ Just (safeDecode name, address)
        f _ = return Nothing
        defs = case Maintainers.getRootDefs patternsCompiled of
                   Just (Just ls) -> ls
                   _ -> []
    m <- mapM f (map snd defs)
    return $ Map.fromList $ catMaybes m

githashRepr :: (MonadGitomail m) => GIT.GitCommitHash -> m Text
githashRepr githash = do
    config <- getConfig
    return $ T.take (config CFG.^.|| CFG.hashSize) githash

getVersion :: Text
getVersion = T.concat [T.pack $ showVersion version, V.version]

getFooter :: (MonadGitomail m) => m F.FList
getFooter = do
    config <- getConfig

    let v = [F.TPlain getVersion]
        v' = maybe v (const []) $ config ^. CFG.hashMap

    return $ F.mkForm F.Footer $
                  [F.TPlain "Sent by ",
                   F.TForm (F.Link "http://github.com/kernelim/gitomail")
                       $ F.mkFormS F.Dark $ F.mkPlain "Gitomail",
                   F.TPlain " "] ++ v' ++ [F.TPlain "\n"]

mapCommitHash :: (MonadGitomail m) => GIT.GitCommitHash -> m (GIT.GitCommitHash)
mapCommitHash h = do
    config <- getConfig
    return $  maybe h (\m -> fromMaybe h (Map.lookup h m)) (config ^. CFG.hashMap)

randomNumbers :: Int -> StdGen -> [Char]
randomNumbers count g = randomBytes count g
  where
    randomBytes 0 _     = []
    randomBytes count' g' =
            (chr ((ord '0') + (value `mod` 10))):randomBytes (count' - 1) nextG
        where
            (value, nextG) = next g'

genExtraEMailHeaders :: (MonadGitomail m) => Address -> m [(BS8.ByteString, Text)]
genExtraEMailHeaders (Address _ email) = do
    config <- getConfig

    let v = case config ^. CFG.hashMap of
               Nothing -> T.concat [T.pack $ showVersion version, " ", V.version]
               Just _ -> "0.0.0"

    UnixTime secs nsecs <- liftIO getUnixTime
    numbers <- case config ^. CFG.hashMap of
        Nothing -> do
            g <- liftIO $ Rand.getStdGen
            return $ T.pack $ show secs ++ "-" ++ show nsecs ++ "-" ++ (randomNumbers 10 g)
        Just _ -> do
            messageId <- fakeMessageId <+= 1
            let testRunId = leadingZeros 7 $ showT $ config CFG.^.|| CFG.testRunId
            return $ "0000000000-" +@ testRunId +@ "-" +@ (leadingZeros 4 $ showT messageId)

    return [
        ("Message-Id", T.concat ["<", numbers, "-gitomail-", email, ">"]),
        ("X-Mailer", T.concat ["gitomail ", v])
      ]
