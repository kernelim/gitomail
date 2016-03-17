{-# LANGUAGE ConstraintKinds #-}
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
  , InvalidCommandOutput(..)
  , parseIssueTrackMentions
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
  , getSortedRefs
  , genExtraEMailHeaders
  , gitCmd
  , githashRepr
  , mapCommitHash
  , matchFiles
  , sortedRefList
  , opts
  , withDB
  ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted    as E
import           Control.Lens.Operators      ((^.), (&), (<+=), (.=))
import           Control.Lens                (makeLenses, use, Lens)
import           Control.Monad               (forM,)
import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (StateT, gets, MonadState)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.DList                  as DList
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Maybe                  (fromMaybe, catMaybes)
import qualified Data.Map                    as Map
import           Data.Typeable               (Typeable)
import           Data.Version                (showVersion)
import           Database.LevelDB.Base       (DB)
import qualified Database.LevelDB.Base       as DB
import           Data.UnixTime               (getUnixTime, UnixTime(..))
import           Data.List                   (groupBy, sortOn, sort)
import           System.FilePath             ((</>), takeBaseName)
import           System.Directory            (canonicalizePath, doesFileExist,
                                              getCurrentDirectory, setCurrentDirectory)
import           System.Exit                 (ExitCode (..))
import           System.Environment          (getEnv)
import           System.Random               as Rand
import           Data.Char                   (chr, ord)
import           Text.Regex.TDFA             ((=~))
import           Text.Regex.TDFA.Text        ()
import           Network.Mail.Mime           (Address (..))
import           Git                         (withRepository)
import qualified Git                         as Git
import           Git.Libgit2                 (lgFactory)
import           Data.Time.LocalTime         (zonedTimeToUTC)
----
import           Paths_gitomail              (version)
import qualified Gitomail.Config             as CFG
import           Gitomail.Config             ((^.||))
import qualified Gitomail.Maintainers        as Maintainers
import qualified Gitomail.Opts               as O
import qualified Gitomail.Version            as V
import           Lib.EMail                   (parseEMail', InvalidEMail(..))
import qualified Lib.Formatting              as F
import qualified Lib.Git                     as GIT
import           Lib.Monad                   (lSeqForM)
import           Lib.Text                    ((+@), showT, leadingZeros,
                                              safeDecode)
import           Lib.Process                 (readProcess, readProcess'')
import           Lib.Regex                   (matchWhole, splitByCaptures)
------------------------------------------------------------------------------------

type GitRefList = [(O.GitRef, GIT.CommitHash)]

data Gitomail = Gitomail {
    opts                :: O.Opts
  , _fakeMessageId      :: Int
  , _sortedRefList      :: Maybe (FilePath, [GitRefList])
  , __loadFiles         :: Maybe (O.GitRef, (GIT.Tree (Maybe BS8.ByteString)))
  , __parseFiles        :: Maybe (O.GitRef, (GIT.Tree (Maybe Maintainers.Unit)))
  , __compilePatterns   :: Maybe (O.GitRef, (GIT.Tree (Maybe [Maintainers.DefInFile])))
  , __matchFiles        :: Maybe (O.GitRef, ((GIT.Tree Maintainers.AssignedFileStatus, Maintainers.MatchErrors)))
  , __getExtraCCTo      :: Maybe ((), ([Address], [Address]))
  , __getRepositoryPath :: Maybe ((), FilePath)
  , __getConfig         :: Maybe ((), CFG.Config)
  , __getFromEMail      :: Maybe ((), Address)
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

data InvalidCommandOutput = InvalidCommandOutput String deriving (Typeable)
instance E.Exception InvalidCommandOutput
instance Show InvalidCommandOutput where
    show (InvalidCommandOutput msgstr) = "InvalidCommandOutput: " ++ msgstr

getGitomail :: O.Opts -> IO (Gitomail)
getGitomail opts = do
    return $ Gitomail opts 0 Nothing Nothing Nothing Nothing Nothing
                             Nothing Nothing Nothing Nothing

cacheInStateBySomething :: (MonadState c m, Eq b)
                       => (Lens c c (Maybe (b, a)) (Maybe (b, a)))
                       -> m a
                       -> b
                       -> m a
cacheInStateBySomething accessor act value = do
    srl <- use accessor
    let cacheMiss =
          do res <- act
             accessor .= Just (value, res)
             return res
    case srl of
        Nothing -> cacheMiss
        Just (inp, res) ->
            do if inp == value
                   then return res
                   else cacheMiss

cacheByRepoPathname :: (MonadGitomail m)
                       => (Lens Gitomail Gitomail (Maybe (FilePath, a)) (Maybe (FilePath, a)))
                       -> m a
                       -> m a
cacheByRepoPathname accessor act = do
    getRepositoryPath >>= cacheInStateBySomething accessor act

class (MonadIO m, MonadState Gitomail m, MonadBaseControl IO m, MonadMask m) => MonadGitomail m where
instance (MonadIO m, MonadBaseControl IO m, MonadMask m) => MonadGitomail (StateT Gitomail m) where

loadFiles :: (MonadGitomail m) => O.GitRef -> m (GIT.Tree (Maybe BS8.ByteString))
loadFiles gitref = do
    repoPath <- getRepositoryPath
    let act = Maintainers.loadFiles repoPath gitref
    cacheInStateBySomething _loadFiles act gitref

parseFiles :: (MonadGitomail m) => O.GitRef -> m (GIT.Tree (Maybe Maintainers.Unit))
parseFiles gitref = do
    let act = loadFiles gitref >>= Maintainers.parseFiles
    cacheInStateBySomething _parseFiles act gitref

compilePatterns :: (MonadGitomail m) => O.GitRef -> m (GIT.Tree (Maybe [(Int, Maintainers.Definition)]))
compilePatterns gitref = do
    let act = parseFiles gitref >>= Maintainers.compilePatterns
    cacheInStateBySomething _compilePatterns act gitref

matchFiles   :: (MonadGitomail m) => O.GitRef -> m (GIT.Tree Maintainers.AssignedFileStatus,
                                                    Maintainers.MatchErrors)
matchFiles gitref = do
    let act = do patterns <- compilePatterns gitref
                 Maintainers.matchFiles (Maintainers.assignDefinitionFiles patterns)
    cacheInStateBySomething _matchFiles act gitref

getRepositoryPath :: (MonadGitomail m) => m FilePath
getRepositoryPath = cacheInStateBySomething _getRepositoryPath act ()
    where
        act = do
            opts <- gets opts
            liftIO $ do
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

getConfig         :: (MonadGitomail m) => m CFG.Config
getConfig         = cacheInStateBySomething _getConfig act ()
    where
        act = do
            opts <- gets opts
            repoPath <- getRepositoryPath
            liftIO $ do
                let safeGetEnv v =
                        E.catch (fmap Just $ getEnv v) (\(_ :: E.SomeException) -> return Nothing)
                    existence e = fmap (\case True -> [e] ; False -> []) $ doesFileExist e
                    homeConfig' =
                        safeGetEnv "HOME" >>= \case
                            Nothing -> return []
                            Just homePath -> existence $ homePath </> ".gitomailconf.yaml"
                    repoConfig' = do
                        existence $ repoPath </> "gitomailconf.yaml"
                    checkOpt a = if opts ^. O.noImplicitConfigs then return [] else a

                homeConfig <- checkOpt homeConfig'
                repoConfig <- checkOpt repoConfig'

                case homeConfig ++ repoConfig ++ opts ^. O.configPaths of
                    []    -> (E.throw $ ParameterNeeded $ BS8.unpack "config paths")
                    paths -> fmap CFG.final $ fmap (foldl1 CFG.combine) $ forM paths $ CFG.parse

getFromEMail      :: (MonadGitomail m) => m Address
getFromEMail      = do
        cacheInStateBySomething _getFromEMail act ()
    where
        act = do
          config <- getConfig
          case config ^. CFG.fromEMail of
              Nothing  -> E.throw $ ParameterNeeded $ BS8.unpack "from_email"
              Just fromEMail ->
                  case parseEMail' fromEMail of
                      Left r -> E.throw $ InvalidEMail $ "from_email: " ++ r
                      Right r -> return r

getExtraCCTo      :: (MonadGitomail m) => m ([Address], [Address])
getExtraCCTo      = do
        cacheInStateBySomething _getExtraCCTo act ()
    where
        act = do
            opts <- gets opts
            [cc, to] <- forM [(O.extraCC, "CC"), (O.extraTo, "To")] $ \(getter, name) -> do
                forM (map parseEMail' (opts ^. getter)) $ \case
                         Left r -> E.throw $ InvalidEMail $ name ++ ": " ++  r
                         Right r -> return r
            return (cc, to)

getRepoName :: (MonadGitomail m) => m Text
getRepoName = do
    config <- getConfig
    opts <- gets opts
    case (opts ^. O.repositoryName, config ^. CFG.repoName, opts ^. O.repositoryPath) of
        (Just name, _, _) ->
            -- If supplied from command line, take from there.
            return name
        (_, Just name, _) ->
            -- Otherwise taking the repoName from the config file.
            return name
        (_, _, Just repoPath) -> do
            -- If the user-provided repoPath is a working directory that
            -- has .git, we can guess the repo name from its name.
            canon <- liftIO $ canonicalizePath repoPath
            return $ T.pack $ takeBaseName canon
        (_, _, Nothing) -> return "?"

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
    patternsCompiled <- compilePatterns gitRef
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

githashRepr :: (MonadGitomail m) => GIT.CommitHash -> m Text
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

mapCommitHash :: (MonadGitomail m) => GIT.CommitHash -> m (GIT.CommitHash)
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

getRefScoreFunc :: (MonadGitomail m) => m (Text -> Int)
getRefScoreFunc = do
    config <- getConfig
    let rootRefsTexts = config ^.|| CFG.rootRefs
    let rootRefs = map matchWhole rootRefsTexts
    let scoreRef ref = fromMaybe 0 $ lookup True $ zip (rootRefs <*> [ref]) [1..(length rootRefsTexts)]
    return scoreRef

getRefState :: (MonadGitomail m) => m GitRefList
getRefState = do
    refsLines <- fmap T.lines $ gitCmd ["show-ref", "--heads", "--tags", "--dereference"]
    fmap catMaybes $ lSeqForM refsLines $ \line -> do
        let invalid f =
               E.throw $ InvalidCommandOutput ("git show-ref returned: " ++ show f)
        case line =~ ("^([a-f0-9]+) refs/(tags/[^^]+)([\\^]{})?$" :: Text) of
            [[_, _, _, ""]] -> return Nothing
            [[_, hash, name, "^{}"]] -> return $ Just (name, hash)
            x1 -> case line =~ ("^([a-f0-9]+) refs/(heads/.*)$" :: Text) of
                [[_, hash, name]] -> return $ Just (name, hash)
                x2 -> invalid (line, x1, x2)

sortRefsByPriority :: (MonadGitomail m) => GitRefList -> m [GitRefList]
sortRefsByPriority reflist = do
    refsMatcher <- getRefsMatcher
    refScore <- getRefScoreFunc
    path <- getRepositoryPath
    byScores <-
        withRepository lgFactory path $ do
            fmap catMaybes $ lSeqForM reflist $ \(refname, hash) ->
                if refsMatcher refname
                    then do obj <- Git.parseOid hash >>= Git.lookupObject
                            let committerTime commit =
                                    return $ Just $ Git.signatureWhen $ Git.commitCommitter $ commit
                            maybeTimestamp <- case obj of
                                Git.TagObj tag -> (Git.lookupCommit $ Git.tagCommit tag)
                                                   >>= committerTime
                                Git.CommitObj commit -> committerTime commit
                                _ -> return Nothing
                            case maybeTimestamp of
                                Nothing -> return Nothing
                                Just timestamp -> do
                                    return $ Just (refScore refname ,
                                                   (zonedTimeToUTC timestamp, refname, hash))
                    else return Nothing
    let g = groupBy (\x y -> fst x == fst y) $ sortOn ((0 -) . fst) byScores
        h = map (sort . map snd) g
    return $ map (map (\(_, a, b) -> (a, b))) h

getSortedRefs :: (MonadGitomail m) => m [GitRefList]
getSortedRefs = cacheByRepoPathname sortedRefList (getRefState >>= sortRefsByPriority)

parseIssueTrackMentions :: MonadGitomail m =>
                            (Text -> a) -> (Text -> DList.DList a -> a) -> Text -> m (DList.DList a)
parseIssueTrackMentions plain link msg = do
    config <- getConfig
    case (config ^. CFG.issueTrackMatch, config ^. CFG.issueTrackURL) of
        (Just regex, Just url) ->
             do let captures = splitByCaptures regex 1 msg
                let mkUrl r = T.replace "%s" r url
                let perElem (Left str) = plain str
                    perElem (Right str) = link (mkUrl str) $ DList.singleton (plain str)
                return $ DList.fromList $ map perElem captures
        _ -> return $ DList.singleton (plain msg)
