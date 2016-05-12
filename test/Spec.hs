{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted  as E
import           Control.Monad               (void, forM_, forM, when)
import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (StateT, evalStateT, get, MonadState,
                                              modify, gets)
import           Control.Lens                (makeLenses)
import           Control.Lens.Operators      ((<+=))
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Typeable               (Typeable)
import           Data.Maybe                  (catMaybes)
import           Data.List                   (intersperse)
import           Data.Text                   (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as T
import qualified Data.Text.IO              as T
import qualified Data.Map                  as Map
import           Data.Map                   (Map)
import           System.Console.ANSI
import           System.Environment         (setEnv)
import           System.FilePath            ((</>))
import           System.Directory           (setCurrentDirectory,
                                            createDirectory, getCurrentDirectory,
                                            getDirectoryContents, doesFileExist,
                                            removeDirectory)
import           System.IO.Temp             (withSystemTempDirectory)
import           System.Exit                (ExitCode (..), exitWith)
import qualified Data.Yaml                  as Yaml
import           Text.Regex.TDFA            ((=~))
import           Text.Regex.TDFA.Text       ()
----
import           Lib.Process                (readProcess, readProcess'')
import           Lib.Text                   (removeTrailingNewLine, (+@), showT)
import           Lib.LiftedPrelude
import qualified Paths_gitomail             as Paths_gitomail
------------------------------------------------------------------------------------

data UnexpectedState = UnexpectedState String deriving (Typeable)
instance E.Exception UnexpectedState
instance Show UnexpectedState where
  show (UnexpectedState msgstr) = "UnexpectedState: " ++ msgstr

data Context = Context {
      contextStrs     :: (IORef [Text])
    , _testRunId      :: Int
    , sourceDir       :: FilePath
    , contextOutputs  :: (Maybe FilePath)
    , contextDerandom :: (IORef (Map Text Text))
    }

makeLenses ''Context

class (MonadIO m, MonadState Context m, MonadBaseControl IO m, MonadMask m) => MonadSpec m where
instance (MonadIO m, MonadBaseControl IO m, MonadMask m) => MonadSpec (StateT Context m) where

contextNew :: (MonadIO m) => m Context
contextNew = do
    i <- newIORef []
    j <- newIORef Map.empty
    return (Context i 0 "" Nothing j)

msg :: (MonadSpec m) => Text -> m ()
msg x = do ctx <- get
           f ctx x
  where
    f (Context{contextStrs = ctx}) text = do
        lst <- readIORef ctx
        liftIO $ do
            setSGR [SetColor Foreground Vivid Cyan]
            T.putStr $ T.concat (intersperse ":" (reverse lst))
            setSGR [SetColor Foreground Dull Cyan]
            T.putStr $ ": "
            setSGR [Reset]
            T.putStrLn $ T.concat [text]

msgLines :: (MonadSpec m) => Text -> m ()
msgLines t = do
    forM_ (T.lines t) msg

wrap :: (MonadSpec m) => Text -> m b -> m b
wrap title x = do ctx<- get
                  f ctx title x
  where
    f (Context{contextStrs = ctx}) title' act = do
        lst <- readIORef ctx
        writeIORef ctx (title':lst)
        let restore = writeIORef ctx lst
            normal = do
                r <- act
                restore
                return r
            excp (e::E.SomeException) = do
                msg "aborted due to exception"
                restore
                E.throw e
        E.catch normal excp

writeFile' :: (MonadIO m) => FilePath -> Text -> m ()
writeFile' fp a = liftIO $ T.writeFile fp a

readFile' :: (MonadIO m) => FilePath -> m Text
readFile' fp = liftIO $ T.readFile fp

appendFile' :: MonadIO m => FilePath -> Text -> m ()
appendFile' fp content = do
    a <- liftIO $ T.readFile fp
    liftIO $ T.writeFile fp $ T.concat [a, content]

gitomail :: (MonadSpec m) => [Text] -> m Text
gitomail params = do
    bin <- liftIO $ fmap (</> "gitomail") Paths_gitomail.getBinDir
    readProcess bin params

gitomailC :: (MonadSpec m) => Text -> [Text] -> m ()
gitomailC save params = gitomailCconf save params []

gitomailCconf :: (MonadSpec m) => Text -> [Text] -> [(Text, Yaml.Value)] -> m ()
gitomailCconf save params conf = do
    let fp = ".git/gitomail.conf"
    derandoms <- gets contextDerandom >>= readIORef
    curTestRunId <- testRunId <+= 1

    writeFile' fp $ T.decodeUtf8 $ Yaml.encode $ Yaml.object $
        [ "from_email"         Yaml..= Yaml.toJSON ("bot@gitomail.com" :: Text)
        , "hash_map"           Yaml..= Yaml.toJSON (Just derandoms)
        , "test_run_id"        Yaml..= Yaml.toJSON (curTestRunId)
        , "issue_track_match"  Yaml..= Yaml.toJSON ("[[]((PROJECT|OTHER)-[0-9]+)[]]" :: Text)
        , "issue_track_url"    Yaml..= Yaml.toJSON ("https://somefakeproject.com/browse/%s" :: Text)
        , "commit_url"         Yaml..= Yaml.toJSON ("https://github.com/gitomail/%r/commit/%H" :: Text)
        ] ++ conf

    t <- gets contextOutputs >>= \case
        Nothing -> E.throw $ UnexpectedState "no output dir"
        Just outputDir -> do
            let outputDirForThis = outputDir </> (T.unpack save ++ ".mails")
            liftIO $ createDirectory outputDirForThis
            t <- gitomail $ ["-o", T.pack outputDirForThis,
                             "--no-implicit-configs",
                             "-c", fp, "-r", "."] ++ params
            let tD = T.replace (T.pack outputDirForThis) "$TEMP" t
            writeFile' (outputDir </> (T.unpack $ save +@ ".stdout.txt")) tD

            contents <- liftIO $ getDirectoryContents outputDirForThis
            files <- fmap catMaybes $ forM contents $ \filename -> do
                let filenameInDir = outputDirForThis </> filename
                exist <- liftIO $ doesFileExist filenameInDir
                case exist of
                    True -> do  content <- readFile' filenameInDir
                                let regex = "Content-Type: multipart/alternative; boundary=\"([^\"]+)\"\n" :: Text
                                case (content =~ regex) :: [[Text]] of
                                    ([_, sig]:_) -> writeFile' filenameInDir $ T.replace sig "<random-replaced-for-test>" content
                                    _ -> return ()
                                return $ Just filename
                    False -> return Nothing

            when (files == []) $ do liftIO $ removeDirectory outputDirForThis

            return tD

    wrap save $ msgLines t
    return ()

git :: (MonadSpec m) => [Text] -> m Text
git p = do
    msg $ "git " +@ showT p
    readProcess "git" p

git' :: (MonadSpec m) => [Text] -> m ()
git' = void . git

gitP :: (MonadSpec m) => [Text] -> m ()
gitP params =
    do x <- git params
       liftIO $ T.putStrLn x

gitRevParse :: Text -> (MonadSpec m) => m Text
gitRevParse rev = fmap removeTrailingNewLine $ readProcess "git" ["rev-parse", rev]

gitDerandomizeHash :: Text -> (MonadSpec m) => m ()
gitDerandomizeHash hash = do
    currentRev <- gitRevParse hash
    contextDerandomI <- gets contextDerandom

    count <- fmap length $ readIORef contextDerandomI
    let rep c i              = take i $ repeat c
        zeroes               = rep '0'
        ffffff               = rep 'f'
        strCount             = show (1 + count)
        sha1Length           = 40
        n                    = 5
        strCountLeadingZeros = zeroes (n - length strCount) ++ strCount
        strCountFinal        = strCountLeadingZeros ++ (ffffff $ sha1Length - n)
        textCount            = T.pack strCountFinal

    msg $ "Mapping for test " +@ currentRev +@ " to " +@ textCount
    modifyIORef' contextDerandomI (Map.insert currentRev textCount)

gitDerandomizeHashHEAD :: (MonadSpec m) => m ()
gitDerandomizeHashHEAD = gitDerandomizeHash "HEAD"

tests :: (MonadSpec m) => FilePath -> m ()
tests tempDir = do
    Context{..} <- get

    let add        = "add"
        readme     = "README.md"
        file2      = "Maintainers"
        other      = "Other.txt"
        automailer = ["auto-mailer"]
        repoDir    = tempDir </> "repo"
        repo2Dir   = tempDir </> "repo2"

        commit msg' = do
            git' ["commit", "-m", msg']
            gitDerandomizeHashHEAD

        fileAppend' file i = do
            appendFile' (T.unpack file) $ "Added content " +@ showT (i :: Int) +@ "\n"
            git' [add, file]

        fileAppend msg' file i = do
            fileAppend' file i
            commit $ "Updating " +@ file +@ " [" +@ showT i +@ "]" +@ msg'

        takeFile fromFile toFile = do
            readFile' (sourceDir </> "test" </> "source" </> fromFile) >>= writeFile' toFile
            git' [add, T.pack toFile]

        readmeAppendMsg msg'  = fileAppend msg' readme
        readmeAppend          = fileAppend "" readme
        otherAppend           = fileAppend "" other
        backToMaster          = git' ["checkout", "master"]
        checkoutCreate branch = git' ["checkout", "-b", branch]
        removeBranch branch   = git' ["branch", "-D", branch]
        checkout branch       = git' ["checkout", branch]
        rebase branch         = git' ["rebase", branch]
        initRepo r = do
            liftIO $ createDirectory r
            liftIO $ setCurrentDirectory r
            git' ["init"]
            git' ["config", "user.name", "Main User"]
            git' ["config", "user.email", "main@gitomail.com"]
            git' ["config", "core.abbrev", "12"]

    initRepo repoDir

    writeFile' readme "Content\n"
    writeFile' "Maintainers" $
         "alias user  Main User <main@gitomail.com>\n" +@
         "maintainer user\n"
        -- FIXME when throwing InvalidAlias, need to tell why

    git' [add, readme, file2]
    git' ["commit", "-m", "Adding README.md"]
    gitDerandomizeHashHEAD
    --  master: -

    msg "Verifying a simple one commit progression of master"
    ---------------------------------------------------------

    gitomailC "1-auto" automailer
    --  master: *

    readmeAppend 1
    --  master: *-

    gitomailC "2-auto" automailer
    --  master: **

    msg "Verifying for a topic branch without master progression"
    ---------------------------------------------------------

    checkoutCreate "topic"
    forM_ [2..3] readmeAppend
    --  master: a*
    --  topic[master]: --

    gitomailC "3-auto" automailer
    --  master: a*
    --  topic[master]: **

    backToMaster

    msg "Verifying for a topic branch with master progression"
    ---------------------------------------------------------

    forM_ [4..5] $ readmeAppendMsg " [OTHER-4]"
    --  <old>: a*
    --  master[<old>]: --
    --  topic[<old>]: **

    checkoutCreate "topic2"
    writeFile' other "Content"
    forM_ [6..7] otherAppend

    --  <old>: a*
    --  master[<old>]: --
    --  topic[<old>]: **
    --  topic2[master]: --

    backToMaster
    forM_ [8..9] $ readmeAppendMsg " [PROJECT-31]\n\nAlso referring to [OTHER-4]"

    --  <old>: a*
    --  <old2>[<old>]: --
    --  master[<old2>]: --
    --  topic[<old>]: **
    --  topic2[<old2>]: --

    gitomailC "4-auto" automailer

    --  -> <old2>[<old>]: **
    --  -> master[<old2>]: **
    --  -> topic2[<old2>]: **

    msg "Verifying after a topic branch rebase"
    ---------------------------------------------------------

    checkout "topic2"
    rebase "master"

    gitDerandomizeHash "HEAD~1"
    gitDerandomizeHash "HEAD"

    --  -> topic2[<master>]: --

    gitomailC "5-auto" automailer

    --  -> topic2[<master>]: **

    msg "Partial rebase of a topic branch"
    ---------------------------------------------------------

    do beforeAmendHash <- gitRevParse "HEAD"
       fileAppend' other 50
       git' ["commit", "--amend", "-a", "-m", "amended commit"]
       gitDerandomizeHash "HEAD"
       gitomailC "23-auto" automailer
       gitP ["reset", "--hard", beforeAmendHash]
       gitomailC "24-auto" automailer

    msg "Verifying numbering after added commits to topic branch"
    ---------------------------------------------------------

    forM_ [10..11] otherAppend
    --  -> topic2[<master>]: **--
    gitomailC "6-auto" automailer
    --  -> topic2[<master>]: ****

    msg "Verifying numbering in a branch based on an another branch of equal status"
    ---------------------------------------------------------

    checkoutCreate "topic1-2"
    forM_ [14..15] otherAppend

    --  -> topic1-2[<topic2>]: --
    gitomailC "10-auto" automailer
    --  -> topic1-2[<topic2>]: **

    msg "Verifying numbering in a branch based on a removed branch"
    ---------------------------------------------------------

    checkoutCreate "topic1-1"
    forM_ [16..17] otherAppend
    removeBranch "topic1-2"

    --  -> topic1-1[<topic2>]: **--
    gitomailC "11-auto" automailer
    --  -> topic1-1[<topic2>]: ****

    msg "Verifying numbering in a branch based on a removed branch [2]"
    ---------------------------------------------------------

    forM_ [17..18] otherAppend
    removeBranch "topic2"

    --  -> topic1-1[<master>]: ******--
    gitomailC "12-auto" automailer
    --  -> topic1-1[<master>]: ********

    msg "Alias Ref Match feature"
    -----------------------------

    checkoutCreate "user/for/other/feature"

    appendFile' "Maintainers" $
         "alias other  Other User <other@gitomail.com>\n"

    git' [add, "Maintainers"]
    forM_ [12..13] readmeAppend
    checkout "master"

    gitomailC "9-auto" automailer

    msg "Source highlight in diff"
    ----------------------------------

    let exts = [".hs", ".c"]
    forM_ exts $ \ext -> do
        takeFile ("test1" ++ ext) ("test" ++ ext)

    commit $ T.concat [ "Adding\n\nDescription\n"]

    gitomailC "13-auto" automailer
    gitomailC "13-show" ["-g", "HEAD", "show-one"]

    forM_ exts $ \ext -> do
        takeFile ("test2" ++ ext) ("test" ++ ext)

    commit $ T.concat [ "Modifing test"]

    gitomailC "14-auto" automailer
    gitomailC "14-show" ["-g", "HEAD", "show-one"]

    msg "Merge commits handling [1]"
    ----------------------------------

    -- A complicated scenario where just a one commit,
    -- a trivial merge one, is added to a ref.

    checkoutCreate "mergetopic-1"
    forM_ [14..15] readmeAppend
    beforeMergeHash <- gitRevParse "HEAD"
    checkout "master"
    gitP ["merge", "--no-ff", "mergetopic-1"]
    removeBranch "mergetopic-1"
    gitDerandomizeHashHEAD
    mergeHash <- gitRevParse "HEAD"
    gitP ["reset", "--hard", beforeMergeHash]
    gitomailC "15-auto" automailer
    gitP ["reset", "--hard", mergeHash]
    gitomailC "16-auto" automailer

    msg "Merge commits handling [2]"
    ----------------------------------

    -- Post about a rebased branch, then the
    -- rebased branch is pushed as-is to
    -- master.

    checkoutCreate "mergetopic-2"
    forM_ [19..21] readmeAppend
    checkout "master"
    gitomailC "17-auto" automailer

    -- Rebasing staging it in another branch, with no-ff.
    writeFile' other "Content"
    forM_ [22..24] otherAppend
    checkout "mergetopic-2"
    rebase "master"
    gitDerandomizeHash "HEAD~2"
    gitDerandomizeHash "HEAD~1"
    gitDerandomizeHash "HEAD"
    checkout "master"
    checkoutCreate "staging"
    gitP ["merge", "--no-ff", "mergetopic-2"]
    gitDerandomizeHashHEAD
    gitomailC "18-auto" automailer

    -- Taking the changes into master
    removeBranch "mergetopic-2"
    checkout "master"
    gitP ["reset", "--hard", "staging"]
    forM_ [25..26] readmeAppend
    gitomailC "19-auto" automailer
    removeBranch "staging"

    msg "Verifying for same commits in several branches"
    ---------------------------------------------------------

    forM_ [27] otherAppend
    checkoutCreate "before-master-lexically"
    forM_ [28..29] otherAppend
    gitP ["branch", "z-after-master", "before-master-lexically"]
    checkout "master"
    gitomailC "20-auto" automailer
    removeBranch "before-master-lexically"
    removeBranch "z-after-master"

    msg "Check email destination filter"
    -------------------------------------

    appendFile' "Maintainers" $ "alias other  Other User <other@gitomail.com>\n"
    appendFile' "Maintainers" $ "reviewer other\n"
    git' [add, "Maintainers"]
    forM_ [30..31] readmeAppend
    gitomailCconf "21-auto" automailer [
        "filtered_email_destinations"  Yaml..= Yaml.toJSON ["other@gitomail.com" :: Text]
        ]

    git' ["reset", "--hard", "HEAD~2"]


    msg "One branch has bad Maintainers info, another one doesn't"
    --------------------------------------------------------------

    checkoutCreate "branch-a"
    appendFile' "Maintainers" $ "reviewer bla\n"
    git' [add, "Maintainers"]
    forM_ [33] readmeAppend

    checkout "master"
    checkoutCreate "branch-b"
    git' [add, "Maintainers"]
    forM_ [34] readmeAppend
    gitomailC "22-auto" automailer
    checkout "master"
    removeBranch "branch-a"
    removeBranch "branch-b"

    msg "Ref going backward after init"
    ----------------------------------

    initRepo repo2Dir

    writeFile' readme "Content\n"
    writeFile' "Maintainers" $
         "alias user  Main User <main@gitomail.com>\n" +@
         "maintainer user\n"
    forM_ [1..5] readmeAppend

    gitomailC "7-auto" automailer

    git' ["branch", "older", "HEAD~2"]
    git' ["reset", "--hard", "HEAD~3"]

    gitomailC "8-auto" automailer
    checkout "master"

run :: (MonadSpec m) => m ()
run = do
    wrap "main" $ do
        do help <- gitomail ["--help"]
           wrap "help" $ msgLines help

        withSystemTempDirectory "gitomail-test" $ \tempDir -> do
            liftIO $ setEnv "GIT_COMMITTER_DATE" "1400000000 +0000"
            let outputDir = tempDir </> "output"

            cur <- liftIO $ getCurrentDirectory
            modify (\r -> r {contextOutputs = Just outputDir,
                             sourceDir = cur})

            liftIO $ createDirectory outputDir
            tests tempDir
            liftIO $ setCurrentDirectory cur

            let actual = T.pack outputDir
            let actualCopy = "test/actual"
            let expected = "test/expected"

            (rc, x, y) <- readProcess'' "diff" ["-ur", expected, actual] ""
            case rc of
                ExitSuccess -> do
                    msg "All seems good!"
                    return ()
                _ -> do msg "-------------------------------------------------------------------"
                        msg "Found difference between expected output and actual output"
                        msg ""
                        msgLines x
                        msgLines y
                        msg ""
                        msg $ T.concat ["Actual outputs copied to ", actualCopy, "."]
                        msgLines $ T.concat ["If they are okay, then commit them to test/expected:\n",
                                             "    rm -rf test/expected && mv test/actual test/expected && git add test/expected"]
                        _ <- readProcess "rm" ["-rf", actualCopy]
                        _ <- readProcess "cp" ["-a", actual, actualCopy]
                        liftIO $ exitWith rc

            return ()

main :: IO ()
main = do
    putStrLn ""
    void $ contextNew >>= evalStateT run

