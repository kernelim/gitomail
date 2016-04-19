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
    CommitContentInfo(..),
    getCommitInfo,
    ciToMaybeMailInfo,
    sendMailSession,
    sendMails,
    sendOne,
    showOne,
    mapCommitHash,
    putInexactDiffHashInDB
    ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted    as E
import           Control.Lens.Operators      ((&), (^.))
import           Control.Monad               (forM, forM_, when)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.State.Strict  (gets)
import qualified Crypto.Hash.SHA1            as SHA1
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as Base16
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as BL
import qualified Data.DList                  as DList
import           Data.Either                 (rights)
import           Data.List                   (intersperse, (\\),
                                              union)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes, fromMaybe)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Text.Internal.Search   as TI
import qualified Data.Text.IO                as T
import qualified Data.Text.Lazy              as TL
import           Data.Typeable               (Typeable)
import           Database.LevelDB.Base       (DB)
import qualified Database.LevelDB.Base       as DB
import qualified Fancydiff.Formatting        as F
import qualified Fancydiff.HTMLFormatting    as F
import qualified Fancydiff.Themes            as F
import qualified Fancydiff.Lib               as FL
import           Git                         (withRepository)
import           Git.Libgit2                 (lgFactory)
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
import qualified Lib.InlineFormatting        as FI
import qualified Lib.Formatting              as FI
import           Lib.LiftedPrelude
import           Lib.Regex                   (matchWhole)
import           Lib.Text                    (removeTrailingNewLine, safeDecode)
------------------------------------------------------------------------------------

data InvalidDiff = InvalidDiff String deriving (Typeable)
instance E.Exception InvalidDiff
instance Show InvalidDiff where
    show (InvalidDiff msgstr) = "InvalidDiff: " ++ msgstr

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

addIssueTrackLinks :: (MonadGitomail m) => Text -> m F.FList
addIssueTrackLinks msg = parseIssueTrackMentions F.TPlain (\a b -> F.TForm (F.Link a) b) msg

listIssueTrackLinks :: (MonadGitomail m) => Text -> m (Maybe F.FList, [Issue])
listIssueTrackLinks msg = do
    links <- addIssueTrackLinks msg
    let f (F.TPlain _) = Nothing
        f y = Just y
    let l = catMaybes $ map f $ DList.toList links

    let g (Left x) = Just x
        g (Right _) = Nothing
    parse <- parseIssueTrackMentions Left (\_ b -> Right $ g $ head $ DList.toList b) msg
    let commaSep = case l of
            []  -> Nothing
            [_] -> Just $ DList.fromList $ [F.TPlain "Issue: "] ++ l
            _   -> Just $ DList.fromList $ [F.TPlain "L: "] ++ (intersperse (F.TPlain ", ") l)

    return (commaSep, catMaybes $ rights $ DList.toList parse)

type InexactDiffHash = BS.ByteString
type SubjectLine = Text

data MailInfo = MailInfo {
        miMail    :: Mail
      , miSubject :: SubjectLine
    }

data CommitContentInfo = CommitContentInfo {
        cciInexactDiffHash    :: InexactDiffHash
      , cciInexactDiffHashNew :: Bool
      , cciFormatted1         :: F.FList
      , cciFormatted2         :: FI.FList
      , cciMail               :: MailInfo
    }

data CommitInfo = CommitInfo {
        ciAuthorName    :: Text
      , ciCommitSubject :: Text
      , ciContent       :: Either String CommitContentInfo
    }

ciToMaybeMailInfo :: CommitInfo -> Maybe MailInfo
ciToMaybeMailInfo (CommitInfo _ _ (Right (CommitContentInfo _ _ _ _ mi))) = Just mi
ciToMaybeMailInfo _ = Nothing

data CommitMailKind = CommitMailSummary | CommitMailFull
    deriving Eq

getCommitInfo :: (MonadGitomail m)
                     => CommitMailKind
                     -> DB
                     -> O.GitRef
                     -> GIT.CommitHash
                     -> Maybe Int
                     -> m CommitInfo
getCommitInfo cmk db ref commitHash maybeNr = do
    opts <- gets opts
    let debug s = when (opts ^. O.verbose) $ do
                      liftIO $ T.putStrLn $ T.concat ["getCommitInfo: ", T.pack s ]
    debug $ "ref: " ++ show ref ++ " hash: " ++ show commitHash

    (authorName, commitSubjectLine, authorEMail, parentHashesStr) <- do
        let keys = intersperse commitHash
                       ["%aN", "%s", "%ae", "%P"]
        let params = ["show", commitHash, T.concat $ "--pretty=":keys, "-s"]
        commitData <- fmap removeTrailingNewLine $ gitCmd params
        case T.splitOn commitHash commitData of
            [a,b,c,d] -> return (a,b,c,d)
            _ -> E.throw $ InvalidCommandOutput
                   $ "Unexpected Git output (params: " ++ show params ++ ", output: " ++ show commitData ++ ")"

    let returnCommitInfo eitherContent =
            return $ CommitInfo authorName commitSubjectLine eitherContent
        parentHashes =
            if parentHashesStr == "" then [] else T.splitOn " " parentHashesStr
        formatPatchArgsEither =
            case parentHashes of
                   []       -> Right (Nothing, ["--root", commitHash])
                   [parent] -> Right (Just parent, [T.concat [ commitHash, "~1..", commitHash]])
                   _        -> Left "Skipping Merge commits"

    patchEither <- case formatPatchArgsEither of
        Left s ->
            return $ Left s
        Right (maybeParentHash, formatPatchArgs) -> do
            patch <- gitCmd $ ["format-patch", "-M", "--stdout",
                               "--full-index"] ++ formatPatchArgs
            return $ Right (patch, maybeParentHash)

    case patchEither of
        Left s -> returnCommitInfo $ Left s
        Right ("", _) -> returnCommitInfo $ Left $ "Empty commit: " ++ (show commitHash)
        Right (patch, maybeParentHash) -> do
            config <- getConfig
            (commitMessageBody, diff, footer') <-
                 case (TI.indices "\n\n" patch,
                       TI.indices "\ndiff " patch,
                       TI.indices "\n---\n" patch,
                       TI.indices "\n-- \n" patch) of
                         ([], _, _ , _ ) -> E.throw $ InvalidCommandOutput ("End of headers not found: " ++ show commitHash)
                         (_,  _, [], _ ) -> E.throw $ InvalidCommandOutput ("End of commit messages not found: " ++ show commitHash)
                         (_,  _, _ , []) -> E.throw $ InvalidCommandOutput ("End signature not found: " ++ show commitHash)
                         (x', d', y', z')  -> do
                             let x = head x'
                                 d = foldl1 min d'
                                 y = if not (null d') then last $ [b | b <- y', b < d] else head y'
                                 z = last z'
                             when (y < x) $ E.throw $ InvalidCommandOutput $ show (y, x)
                             when (z < y) $ E.throw $ InvalidCommandOutput $ show (z, y)

                             let (_, rest)                  = T.splitAt (x)          patch
                                 (commitMessageBody, rest2) = T.splitAt (y - x)      rest
                                 (diff, footer')            = T.splitAt (z - y)      rest2

                             return (commitMessageBody, diff, footer')

            let footer =
                    case config ^. CFG.hashMap of
                       Nothing -> footer'
                       Just _  -> "\n-- \nx.x.x\n\n"

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

            affectedPathsStr <- gitCmd ["show", commitHash, "--pretty=format:", "--name-only"]
            let affectedPaths = affectedPathsStr & T.encodeUtf8 & BS8.lines

            containedInBranchesList <- case cmk of
                CommitMailFull -> getBranchesContainingCommit commitHash
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

            debug $ "iterating maintainers"
            let refmfilter = (commitHash, Just affectedPaths)
            (matched, matchErrors) <- matchFiles refmfilter
            debug $ "done matching"

            maintainerInfo <- iterateFilesWithMaintainers matched $ \_ i ->
                return $ GIT.treeVal i

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
                    case containedInBranchesList of
                       (x:_) -> x
                       [] -> "<?>"

            let Maintainers.AssignedFileStatus {..} = maintainerInfo
                getEMail (_, email) = E.catch (parseEMail (safeDecode email) >>= (return . Just))
                                        (\(_ :: InvalidEMail) -> return Nothing)
                flagsMaybe =
                    case flags of
                          [] -> Nothing
                          (_:_) -> Just $ safeDecode $ BS8.concat $ ["Flags: "] ++ (intersperse ", " flags)
                    where flags = catMaybes [diffInexactMatches]

            (maybeIssueTrackLinks, issues) <- listIssueTrackLinks commitSubjectLine
            (toListE, ccList) <- do
                  (extraCc', extraTo) <- getExtraCCTo
                  issuesCc <- fmap Set.unions $ forM issues getJiraCcByIssue
                  let extraCc = extraCc' `union` Set.toList issuesCc

                  maintainerM <- case fsMaintainer of
                      Nothing -> return Nothing
                      Just e -> getEMail e

                  others <- mapM getEMail (fsReviewers ++ fsObservers)
                  aliasTo <- case config ^.|| CFG.aliasRefMatch of
                      Nothing -> return []
                      Just aliasRefRegex -> do
                          aliasMap <- getTopAliases refmfilter
                          fmap catMaybes $ forM (Map.toList aliasMap) $ \(name, email) -> do
                              if matchWhole (aliasRefRegex & T.replace "%a" name) ref
                                 then return (Just email)
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

            debug $ "done handling recipients"

            case toListE of
                Left s -> returnCommitInfo $ Left s
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
                      let htmlformat = (F.mkHtmlFormat F.HTMLInline F.brightBackground)
                               { F.fmtFileLinker = blobInCommitURLFunc }

                      let htmlOnlyHeader = T.concat $ (intersperse "<br>" extraInfo) ++ ["<br>"]
                          extraInfo = catMaybes [
                                  commitURL
                                , (Just $ T.concat $ [ "Branches: ",
                                              T.concat (intersperse ", " containedInBranchesList)])
                                , fmap (F.htmlFormatting htmlformat { F.fmtBlockingEnv = False } ) maybeIssueTrackLinks
                                , flagsMaybe
                              ] ++ case matchErrors of
                                      Maintainers.MatchErrors errors ->
                                          flip map (Set.toList errors) $ \((path, line), err) -> T.concat [
                                              "Warning: " ,
                                              safeDecode path, case path of {"" -> "" ; _ -> "/" }, "Maintainers",
                                              ":", T.pack $ show line, ": ",
                                                  case err of
                                                      Maintainers.InvalidAlias alias -> T.concat ["invalid alias - ", safeDecode alias]
                                                      Maintainers.OverlappingAlias alias prevEmail ->
                                                          T.concat ["overlapping alias - ", safeDecode alias, ", previous E-Mail was ",
                                                                    safeDecode prevEmail]]

                      parsedFLists <- case cmk of
                          CommitMailFull -> do
                              diffHighlighted <-
                                  case config ^.|| CFG.sourceHighlight of
                                      False -> return $ FL.highlight diff
                                      True -> do
                                          path <- getRepositoryPath
                                          withRepository lgFactory path $ do
                                              FL.tryDiffWithSourceHighlight diff

                              messageBodyWithLinks <- addIssueTrackLinks commitMessageBody
                              return $ (DList.singleton $ (F.TForm F.MonospacePar) messageBodyWithLinks)
                                          `DList.append` diffHighlighted
                                          `DList.append` (F.highlightMonospace footer)

                          CommitMailSummary -> do
                              return DList.empty

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
                          html = TL.fromChunks [ htmlOnlyHeader,
                                                 F.htmlFormatting htmlformat parsedFLists,
                                                 FI.flistToInlineStyleHtml emailFooter]
                          plain = TL.fromChunks [ F.flistToText parsedFLists,
                                                  FI.flistToText emailFooter
                                                ]
                          replyTo = Address (Just authorName) authorEMail
                          mailInfo = MailInfo mail subjectLine
                          contentInfo = CommitContentInfo diffInexactHash miInexactDiffHashNew parsedFLists emailFooter mailInfo

                      returnCommitInfo $ Right contentInfo


sendMails :: (MonadGitomail m) => [(IO (), MailInfo)] -> m ()
sendMails mails = do
    config <- getConfig
    if length mails == 0
        then putStrLn $ "No E-Mails to send."
        else sendEmails config
  where sendEmails config = do
            putStrLn $ "Sending E-Mails!"
            opts <- gets opts
            indexI <- newIORef (1 :: Int)
            sendMailSession $ \mconn -> do
                forM_ mails $ \(act, mailinfo) -> do
                    e mconn (opts, indexI) mailinfo
                    liftIO $ act
            where
                e mconn (opts, indexI) ((MailInfo {..})) =
                    case filteredDestEMail miMail of
                        Nothing -> return ()
                        Just filteredEMail -> do
                            case opts ^. O.outputPath of
                                Nothing -> do
                                    case mconn of
                                         Nothing -> do bs <- renderMail' filteredEMail
                                                       BL.putStr bs
                                         Just conn -> do
                                             putStrLn $ "  Sending '" ++ (T.unpack miSubject) ++ "'"
                                             sendMimeMail2 filteredEMail conn
                                Just outputPath -> do
                                    bs <- renderMail' filteredEMail
                                    index <- readIORef indexI
                                    modifyIORef' indexI (+1)
                                    let outputFile = outputPath </> show index -- TODO better filename
                                    putStrLn $ "  Writing " ++ outputFile
                                                   ++ " - '" ++ (T.unpack miSubject) ++ "'"
                                    BS.writeFile outputFile $ BS.concat (BL.toChunks bs)

                filteredDestEMail m@Mail{..} = root
                    where root =
                              case (filteredTo, filteredCc) of
                                  ([], [])   -> Nothing
                                  ([], x:xs) -> Just $ r [x] xs
                                  (x, y)     -> Just $ r x y
                          r x y = m { mailTo = x
                                    , mailCc = y
                                    , mailBcc = f mailBcc
                                    }
                          filteredTo = f mailTo
                          filteredCc = f mailCc
                          f = filter g
                          g (Address _ email) =
                              not (email `elem` (config ^.|| CFG.filteredDestEMails))

justOne :: MonadGitomail m => m CommitInfo
justOne = do
    opts <- gets opts
    let gitRef = opts ^. O.gitRef & fromMaybe "HEAD"
    withDB $ \db -> do
        commitHash <- fmap removeTrailingNewLine $ gitCmd ["show", gitRef, "--pretty=%H", "-s"]
        getCommitInfo CommitMailFull db gitRef commitHash Nothing

sendOne :: (MonadGitomail m) => m ()
sendOne = do
    contentinfo <- justOne
    case ciToMaybeMailInfo contentinfo of
        (Just mailinfo) -> sendMails [(return (), mailinfo)]
        _ -> return ()

showOne :: (MonadGitomail m) => m ()
showOne = do
    opts <- gets opts
    contentinfo <- justOne
    case contentinfo of
        CommitInfo{..} -> do
            case ciContent of
                Left str -> do
                    putStrLn str
                Right (CommitContentInfo{..}) -> do
                    when (opts ^. O.verbose) $ do
                        liftIO $ T.putStr $ F.fshow cciFormatted1
                        liftIO $ T.putStr $ FI.fshow cciFormatted2
                    liftIO $ T.putStr $ F.flistToText $ cciFormatted1
                    liftIO $ T.putStr $ FI.flistToText $ cciFormatted2
                    return ()
