{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiWayIf                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Gitomail.CommitToMail (
    InvalidCommandOutput(..),
    CommitMailKind(..),
    MailInfo(..),
    CommitInfo(..),
    makeOneMailCommit,
    sendMailSession,
    sendMails,
    sendOne,
    mapCommitHash,
    putInexactDiffHashInDB
    ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted    as E
import           Control.Lens.Operators      ((&), (^.))
import           Control.Monad               (forM, forM_)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (get, gets, lift)
import qualified Crypto.Hash.SHA1            as SHA1
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as Base16
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as BL
import           Data.ByteString.Search      as StrSearch
import           Data.Either                 (rights)
import           Data.List                   (intersperse, (\\))
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes, fromMaybe)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as TL
import           Data.Typeable               (Typeable)
import           Database.LevelDB.Base       (DB)
import qualified Database.LevelDB.Base       as DB
import           Network.HaskellNet.Auth     (AuthType (PLAIN))
import           Network.HaskellNet.SMTP     (authenticate, sendMimeMail2)
import           Network.HaskellNet.SMTP.SSL (SMTPConnection,
                                              Settings (sslPort),
                                              defaultSettingsSMTPSSL,
                                              defaultSettingsSMTPSTARTTLS,
                                              doSMTPSSLWithSettings,
                                              doSMTPSTARTTLSWithSettings)
import           Network.Mail.Mime           (Address (..), Mail (..), htmlPart,
                                              plainPart, renderAddress,
                                              renderMail')
import           System.FilePath             ((</>))
import           Text.Regex.TDFA             ((=~))
import           Text.Regex.TDFA.Text        ()
----
import           Gitomail.Config             ((^.||))
import qualified Gitomail.Config             as CFG
import           Gitomail.Gitomail
import qualified Gitomail.Maintainers        as Maintainers
import qualified Gitomail.Opts               as O
import           Gitomail.WhoMaintains
import           Lib.EMail                   (InvalidEMail, emailRegEx,
                                              parseEMail)
import qualified Lib.Git                     as GIT
import qualified Lib.InlineFormatting        as F
import           Lib.LiftedPrelude
import           Lib.Process                 (readProcess')
import           Lib.Regex                   (matchWhole)
import           Lib.Text                    (removeTrailingNewLine, (+@))
------------------------------------------------------------------------------------

data InvalidDiff = InvalidDiff String deriving (Typeable)
instance E.Exception InvalidDiff
instance Show InvalidDiff where
    show (InvalidDiff msgstr) = "InvalidDiff: " ++ msgstr

data InvalidCommandOutput = InvalidCommandOutput String deriving (Typeable)
instance E.Exception InvalidCommandOutput
instance Show InvalidCommandOutput where
    show (InvalidCommandOutput msgstr) = "InvalidCommandOutput: " ++ msgstr

data SMTPFail = SMTPFail String deriving (Typeable)
instance E.Exception SMTPFail
instance Show SMTPFail where
    show (SMTPFail msgstr) = "SMTPFail: " ++ msgstr

sendMailSession :: (MonadGitomail m) => (Maybe SMTPConnection -> IO a) -> m a
sendMailSession f = do
    opts <- gets opts
    config <- getConfig
    if (opts ^. O.dryRun || opts ^. O.outputPath /= Nothing)
        then do liftIO $ f Nothing
        else do let (r, d)            = (\x -> case x of
                                            True -> (doSMTPSTARTTLSWithSettings,
                                                     defaultSettingsSMTPSTARTTLS)
                                            False -> (doSMTPSSLWithSettings,
                                                      defaultSettingsSMTPSSL))
                                                    (config ^.|| CFG.sMTPStartTls)
                    username'         = fmap T.unpack $ config ^. CFG.sMTPUsername
                    password'         = fmap T.unpack $ config ^. CFG.sMTPPassword
                    port              = fromIntegral $ config ^.|| CFG.sMTPPort
                    hostname'         = fmap T.unpack $ config ^. CFG.sMTPHostname

                    maybeVar name var = maybe (E.throw $
                                               ParameterNeeded name) return var

                username <- maybeVar "smtp_username" username'
                hostname <- maybeVar "smtp_hostname" hostname'
                password <- maybeVar "smtp_password" password'

                liftIO $ r hostname (d {sslPort = port}) $ \conn -> do
                  authSucceed <- authenticate PLAIN username password conn
                  if authSucceed
                      then f (Just conn)
                      else E.throw $ SMTPFail "Authentication failed."

gitBranchesContainingCommit :: (MonadGitomail m) => Text -> m [Text]
gitBranchesContainingCommit ref = do
    containedInBranches <- gitCmd ["branch", "--contains", ref]
    let gitBranchWhitespaceRemoval = T.filter (\x -> (not . (elem x)) (" *" :: [Char]))
    return $ containedInBranches
            & gitBranchWhitespaceRemoval & T.lines

inexactDiffWasSentStrDBKey :: InexactDiffHash -> BS8.ByteString
inexactDiffWasSentStrDBKey diff = BS8.concat [ "inexact-diff-sent-",  diff ]

putInexactDiffHashInDB :: MonadIO m => DB -> InexactDiffHash -> m ()
putInexactDiffHashInDB db inexactDiffHash = do
    DB.put db (DB.defaultWriteOptions {DB.sync = True })
        (inexactDiffWasSentStrDBKey inexactDiffHash)
        "true"

checkInexactDiffHashInDB :: MonadIO m => DB -> InexactDiffHash -> m Bool
checkInexactDiffHashInDB db inexactDiffHash = do
    v <- DB.get db DB.defaultReadOptions (inexactDiffWasSentStrDBKey inexactDiffHash)
    case v of
        Just _ -> return True
        Nothing -> return False

type InexactDiffHash = BS.ByteString
type SubjectLine = Text

data CommitInfo = CommitInfo {
        ciAuthorName         :: Text
      , ciInexactDiffHash    :: InexactDiffHash
      , ciInexactDiffHashNew :: Bool
      , ciCommitSubject      :: Text
    }

data MailInfo = MailInfo {
        miMail    :: Mail
      , miSubject :: SubjectLine
    }

data CommitMailKind = CommitMailSummary | CommitMailFull
    deriving Eq

makeOneMailCommit :: (MonadGitomail m)
                     => CommitMailKind
                     -> DB
                     -> O.GitRef
                     -> GIT.GitCommitHash
                     -> Maybe Int
                     -> m (Either String (MailInfo, CommitInfo))
makeOneMailCommit cmk db ref commitHash maybeNr = do
    (authorName, commitSubjectLine, authorEMail, commitMessageBody, parentHashesStr) <- do
        let keys = intersperse commitHash
                     ["%aN", "%s", "%ae", "%b", "%P"]
        commitData <- fmap removeTrailingNewLine $ gitCmd ["show", commitHash, T.concat $ "--pretty=":keys, "-s"]
        case T.splitOn commitHash commitData of
            [a,b,c,d,e] -> return (a,b,c,d,e)
            _ -> E.throw $ InvalidCommandOutput "TODO"

    let parentHashes =
            if parentHashesStr == "" then [] else T.splitOn " " parentHashesStr
        formatPatchArgsEither =
            case parentHashes of
                   []       -> Right (Nothing, ["--root", commitHash])
                   [parent] -> Right (Just parent, [T.concat [ commitHash, "~1..", commitHash]])
                   _        -> Left "Skipping Merge commits"

    case formatPatchArgsEither of
        Left s -> return $ Left s
        Right (maybeParentHash, formatPatchArgs) -> do
            config <- getConfig
            repoPath <- getRepositoryPath
            matched <- matchFiles (repoPath, commitHash)
            patch <- gitCmd $ ["format-patch", "-M", "--stdout", "--no-signature"
                               ] ++ formatPatchArgs

            diff <- let part = f commitMessageBody
                        f "" = "\n---\n"
                        f _ = commitMessageBody
                     in case T.breakOn part patch of
                         (_, "")        -> E.throw $ InvalidCommandOutput "TODO"
                         (_, cPlusDiff) -> return  $ T.drop (T.length part) cPlusDiff

            let diffInexactHash = Base16.encode $ SHA1.hash $ T.encodeUtf8 $ diffInexact
                  where diffInexact =
                          T.concat $ map (\x -> if | "@@ "    `T.isPrefixOf`  x -> "@@"
                                                   | "index " `T.isPrefixOf`  x -> "index"
                                                   | otherwise                  -> x) $ T.lines diff
            (diffInexactMatches, miInexactDiffHashNew) <- do
                b <- checkInexactDiffHashInDB db diffInexactHash
                case b of
                    True -> return $ (Just "InexactDiffDup", not b)
                    False -> return $ (Nothing, not b)

            affectedPaths <- gitCmd ["show", commitHash, "--pretty=format:", "--name-only"]
            let affectedPathsSet = affectedPaths & T.encodeUtf8 & BS8.lines & Set.fromList

            containedInBranchesList <- case cmk of
                CommitMailFull -> gitBranchesContainingCommit commitHash
                CommitMailSummary -> return []

            let ccOrToResults = map d ((commitMessageBody =~ ccRegEx :: [[Text]]))
                   where ccRegEx = T.concat ["\n((C[cC])|To|Signed-off-by): ", emailRegEx] :: Text
                d [_ ,typ ,_ , _, _, name, email, ""] = Right (typ, Just name, email)
                d [_ ,typ ,_ , _, _, "", "", email] = Right (typ, Nothing, email)
                d r@_ = Left r

            extraAddresses <- forM ccOrToResults $ \v' ->
                case v' of
                    Left v -> do putStrLn $ "warning: ignored matched regex:" ++ (show v)
                                 return Nothing
                    Right (_, name, email) -> return $ Just $ Address name email

            maintainerInfo <- iterateFilesWithMaintainers matched $ \path i ->
                return $
                   if path `Set.member` affectedPathsSet
                      then GIT.treeVal i
                      else mempty

            refsMatcher <- getRefsMatcher
            repoName <- getRepoName
            shortHash <- mapCommitHash commitHash >>= githashRepr

            let subjectLine =
                    config ^.|| CFG.commitSubjectLine
                    & T.replace "%r" repoName
                    & T.replace "%n" (maybe "" (\nr -> T.concat [", #", T.pack $ show nr]) maybeNr)
                    & T.replace "%h" shortHash
                    & T.replace "%b" leadingBranch
                    & T.replace "%s" commitSubjectLine
                leadingBranch =
                    case filter refsMatcher $ map ("heads/" +@) containedInBranchesList of
                       (x:_) -> T.drop (T.length "heads/") x
                       [] -> "<?>"

            let Maintainers.AssignedFileStatus {..} = maintainerInfo
                getEMail (_, email) = E.catch (parseEMail (T.decodeUtf8 email) >>= (return . Just))
                                        (\(_ :: InvalidEMail) -> return Nothing)
                (_, p2) = StrSearch.breakAfter "\nSubject: " (T.encodeUtf8 patch)
                (_, commit) = StrSearch.breakOn "\n\n" p2

                flagsMaybe =
                    case flags of
                          [] -> Nothing
                          (_:_) -> Just $ T.decodeUtf8 $ BS8.concat $ ["Flags: "] ++ (intersperse ", " flags)
                    where flags = catMaybes [diffInexactMatches]

            (toListE, ccList) <- do
                  (extraCc, extraTo) <- getExtraCCTo
                  maintainerM <- case fsMaintainer of
                      Nothing -> return Nothing
                      Just e -> getEMail e

                  others <- mapM getEMail (fsReviewers ++ fsObservers)
                  aliasTo <- case config ^.|| CFG.aliasRefMatch of
                      Nothing -> return []
                      Just aliasRefRegex -> do
                          aliasMap <- getTopAliases commitHash
                          fmap catMaybes $ forM (Map.toList aliasMap) $ \(name, email) -> do
                              if matchWhole (aliasRefRegex & T.replace "%a" name) ref
                                 then return $ Just email
                                 else return Nothing
                  return $
                      let otherAddresses =
                              -- FIXME: unique over the address only and not the name
                              Set.toList $ Set.fromList $ others ++ (extraAddresses)
                          ccList = catMaybes $ map Just extraCc ++ otherAddresses
                          toList = extraTo ++ aliasTo ++ (catMaybes [maintainerM])
                       in case (cmk, toList, ccList) of
                          (CommitMailFull    , [],   [])   ->
                              (Left $ "skipped - no destination for commit '" ++ T.unpack subjectLine ++ " '", [])
                          (_                 , [],   x:xs) -> (Right (x:xs), [])
                          (_                 , _,   _)    -> (Right toList, ccList)

            case toListE of
                Left s -> return $ Left s
                Right toList ->
                   do commitHash' <- mapCommitHash commitHash
                      parentCommitHash' <- case maybeParentHash of
                          Nothing -> mapCommitHash commitHash
                          Just parentHash -> mapCommitHash parentHash
                      commitURL <- getCommitURL commitHash'
                      blobInCommitURL <- getBlobInCommitURL

                      let blobInCommitURLFunc = fmap f blobInCommitURL
                          f z True filename = z commitHash' filename
                          f z False filename = z parentCommitHash' filename

                      let htmlOnlyHeader = T.concat $ (intersperse "<br>" extraInfo) ++ ["<br>"]
                          extraInfo = catMaybes [
                                  commitURL
                                , (Just $ T.concat $ [ "Branches: ",
                                              T.concat (intersperse ", " containedInBranchesList)])
                                , flagsMaybe
                              ]

                      parsedFLists <- case cmk of
                          CommitMailFull -> do
                              fp <- getDataFile "extra/diff-highlight"
                              patchHighlighted <- readProcess' "perl" [T.pack fp] diff

                              let parseResult = do
                                      patchHighlightedFlist <- F.ansiToFList patchHighlighted
                                      x <- F.combineFLists (patchHighlightedFlist) (F.highlightDiff diff)
                                      return $ (F.highlightMonospace commitMessageBody) ++ x

                              case parseResult of
                                  Right pp -> return pp
                                  Left s -> E.throw $ InvalidDiff s
                          CommitMailSummary -> do
                              return []

                      emailFooter <- getFooter
                      emailAddress <- getFromEMail

                      let Address _ actualSenderEmail = emailAddress
                          fromAddress = Address (Just authorName) actualSenderEmail

                      extraHeaders <- genExtraEMailHeaders fromAddress

                      let mail = Mail
                            { mailFrom = fromAddress
                            , mailTo = toList
                            , mailCc = ccList \\ toList
                            , mailBcc = []
                            , mailHeaders = extraHeaders ++
                                            [("Reply-to", renderAddress replyTo),
                                             ("Subject", subjectLine)]
                            , mailParts = [[plainPart plain, htmlPart html]]
                            }
                          flists = parsedFLists ++ emailFooter
                          html = TL.fromChunks [ htmlOnlyHeader, F.flistToInlineStyleHtml blobInCommitURLFunc flists ]
                          plain = TL.fromChunks [ T.decodeUtf8 commit ]
                          replyTo = Address (Just authorName) authorEMail
                          commitInfo =
                              CommitInfo authorName diffInexactHash miInexactDiffHashNew commitSubjectLine

                      return $ Right $ (MailInfo mail subjectLine, commitInfo)


sendMails :: (MonadGitomail m) => [(IO (), Either String MailInfo)] -> m ()
sendMails mails = do
    if length (rights $ map snd mails) == 0
        then putStrLn $ "No E-Mails to send."
        else sendEmails
  where sendEmails = do
            putStrLn $ "Sending E-Mails!"
            opts <- gets opts
            indexI <- newIORef (1 :: Int)
            sendMailSession $ \mconn -> do
                forM_ mails $ \(act, mailinfo) -> do
                    e mconn (opts, indexI) mailinfo
                    liftIO $ act
            where
                e mconn (opts, indexI) (Right (MailInfo {..})) = do
                    case opts ^. O.outputPath of
                        Nothing -> do
                            case mconn of
                                 Nothing -> do bs <- renderMail' miMail
                                               BL.putStr bs
                                 Just conn -> do
                                     putStrLn $ "  Sending '" ++ (T.unpack miSubject) ++ "'"
                                     sendMimeMail2 miMail conn
                        Just outputPath -> do
                            bs <- renderMail' miMail
                            index <- readIORef indexI
                            modifyIORef' indexI (+1)
                            let outputFile = outputPath </> show index -- TODO better filename
                            putStrLn $ "  Writing " ++ outputFile
                                           ++ " - '" ++ (T.unpack miSubject) ++ "'"
                            BS.writeFile outputFile $ BS.concat (BL.toChunks bs)
                e _ _ (Left msg) = do
                    putStrLn $ "  Skipping,  " ++ msg

sendOne :: (MonadGitomail m) => m ()
sendOne = do
    opts <- gets opts
    let gitRef = opts ^. O.gitRef & fromMaybe "HEAD"
    mailinfo <- withDB $ do
        db <- get
        lift $ do
            commitHash <- fmap removeTrailingNewLine $ gitCmd ["show", gitRef, "--pretty=%H", "-s"]
            makeOneMailCommit CommitMailFull db gitRef commitHash Nothing
    sendMails [(return (), fmap fst mailinfo)]
