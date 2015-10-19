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

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted  as E
import           Control.Monad               (void, forM_, forM, when)
import           Control.Monad.Catch         (MonadMask)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (StateT, evalStateT, get, MonadState,
                                              modify, gets)
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
      _contextStrs    :: (IORef [Text])
    , contextOutputs  :: (Maybe FilePath)
    , contextDerandom :: (IORef (Map Text Text))
    }
class (MonadIO m, MonadState Context m, MonadBaseControl IO m, MonadMask m) => MonadSpec m where
instance (MonadIO m, MonadBaseControl IO m, MonadMask m) => MonadSpec (StateT Context m) where

contextNew :: (MonadIO m) => m Context
contextNew = do
    i <- newIORef []
    j <- newIORef Map.empty
    return (Context i Nothing j)

msg :: (MonadSpec m) => Text -> m ()
msg x = do ctx <- get
           f ctx x
  where
    f (Context ctx _ _) text = do
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
    f (Context ctx _ _) title' act = do
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
gitomailC save params = do
    let fp = ".git/gitomail.conf"
    derandoms <- gets contextDerandom >>= readIORef

    writeFile' fp $ T.decodeUtf8 $ Yaml.encode $ Yaml.object
        [ "from_email"    Yaml..= Yaml.toJSON ("bot@gitomail.com" :: Text)
        , "hash_map"    Yaml..= Yaml.toJSON (Just derandoms)
        , "commit_url"  Yaml..= Yaml.toJSON ("https://github.com/gitomail/%r/commit/%H" :: Text)
        ]

    t <- gets contextOutputs >>= \case
        Nothing -> E.throw $ UnexpectedState "no output dir"
        Just outputDir -> do
            let outputDirForThis = outputDir </> (T.unpack save ++ ".mails")
            liftIO $ createDirectory outputDirForThis
            t <- gitomail $ ["-o", T.pack outputDirForThis,
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

gitDerandomizeHash :: Text -> (MonadSpec m) => m ()
gitDerandomizeHash hash = do
    currentRev <- fmap removeTrailingNewLine $ readProcess "git" ["rev-parse", hash]
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

tests :: (MonadSpec m) => m ()
tests = do
    let add = "add"
        commit = "commit"
        readme = "README.md"
        file2 = "Maintainers"
        other = "Other.txt"
        automailer = ["auto-mailer"]

    let fileAppend file i = do
            appendFile' (T.unpack file) $ "Added content " +@ showT (i :: Int) +@ "\n"
            git' [add, file]
            git' [commit, "-m", "Updating " +@ file +@ " [" +@ showT i +@ "]" ]
            gitDerandomizeHashHEAD
        readmeAppend = fileAppend readme
        otherAppend = fileAppend other
        backToMaster = git' ["checkout", "master"]
        checkoutCreate branch = git' ["checkout", "-b", branch]
        checkout branch = git' ["checkout", branch]
        rebase branch = git' ["rebase", branch]

    writeFile' readme "Content\n"
    writeFile' "Maintainers" $
         "alias user  Main User <main@gitomail.com>\n" +@
         "maintainer user\n"
        -- FIXME when throwing InvalidAlias, need to tell why

    git' [add, readme, file2]
    git' [commit, "-m", "Adding README.md"]
    gitDerandomizeHashHEAD

    msg "Verifying a simple one commit progression of master"
    ---------------------------------------------------------

    gitomailC "1-auto" automailer

    readmeAppend 1

    gitomailC "2-auto" automailer

    msg "Verifying for a topic branch without master progression"
    ---------------------------------------------------------

    checkoutCreate "topic"
    forM_ [2..3] readmeAppend
    gitomailC "3-auto" automailer

    backToMaster

    msg "Verifying for a topic branch with master progression"
    ---------------------------------------------------------

    forM_ [4..5] readmeAppend
    checkoutCreate "topic2"
    writeFile' other "Content"
    forM_ [6..7] otherAppend
    backToMaster
    forM_ [8..9] readmeAppend

    gitomailC "4-auto" automailer

    msg "Verifying after a topic branch rebase"
    -------------------------------------------

    checkout "topic2"
    rebase "master"

    gitDerandomizeHash "HEAD~1"
    gitDerandomizeHash "HEAD"

    gitomailC "5-auto" automailer

    msg "Verifying numbering after added comits to topic branch"
    ------------------------------------------------------------

    forM_ [10..11] otherAppend
    gitomailC "6-auto" automailer

run :: (MonadSpec m) => m ()
run = do
    wrap "main" $ do
        do help <- gitomail ["--help"]
           wrap "help" $ msgLines help

        withSystemTempDirectory "gitomail-test" $ \tempDir -> do
            let repoDir = tempDir </> "repo"
            let outputDir = tempDir </> "output"

            modify (\r -> r {contextOutputs = Just outputDir})

            cur <- liftIO $ do
                cur <- getCurrentDirectory
                createDirectory outputDir
                createDirectory repoDir
                setCurrentDirectory repoDir
                return cur

            git' ["init"]
            git' ["config", "user.name", "Main User"]
            git' ["config", "user.email", "main@gitomail.com"]
            git' ["config", "core.abbrev", "12"]

            tests

            liftIO $ setCurrentDirectory cur

            let actual = T.pack outputDir
            let actualCopy = "test/actual"
            let expected = "test/expected"

            (rc, x, y) <- readProcess'' "diff" ["-ur", expected, actual] ""
            case rc of
                ExitSuccess -> do
                    msg "All seems good!"
                    return ()
                _ -> do msg "Found difference between expected output and actual output"
                        msg ""
                        msgLines x
                        msgLines y
                        msg ""
                        msg $ T.concat ["Actual outputs copied to ", actualCopy, "."]
                        msgLines $ T.concat ["If they are okay, then commit them to test/expected:\n",
                                             "    rm -rf test/expected && mv test/actual test/expected"]
                        _ <- readProcess "rm" ["-rf", actualCopy]
                        _ <- readProcess "cp" ["-a", actual, actualCopy]
                        liftIO $ exitWith rc

            return ()

main :: IO ()
main = do
    putStrLn ""
    void $ contextNew >>= evalStateT run

