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
import qualified Data.DList                  as DList
import qualified Crypto.Hash.SHA1            as SHA1
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16      as Base16
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as BL
import           Data.List                   (intersperse, (\\))
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes, fromMaybe)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T
import qualified Data.Text.Encoding          as T
import           Data.Text.Internal.Search   as TI
import qualified Data.Text.Lazy              as TL
import qualified Data.Array                  as A
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
import           Gitomail.Highlight          (getHighlighter)
import           Gitomail.WhoMaintains
import           Lib.EMail                   (InvalidEMail, emailRegEx,
                                              parseEMail)
import qualified Lib.Git                     as GIT
import qualified Lib.Formatting              as F
import qualified Lib.InlineFormatting        as F
import qualified Lib.AnsiFormatting          as FA
import qualified Lib.DiffHighlight           as DH
import           Lib.LiftedPrelude
import           Lib.DList                   (dlistConcat)
import           Lib.Monad                   (dlistForM)
import           Lib.Regex                   (matchWhole, (=~+))
import           Lib.Text                    (removeTrailingNewLine, (+@),
                                              safeDecode)
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

gitBranchesContainingCommit :: (MonadGitomail m) => Text -> m [Text]
gitBranchesContainingCommit ref = do
    containedInBranches <- gitCmd ["branch", "--contains", ref]
    sortedRefs <- fmap (map fst . concat) $ getSortedRefs

    let gitBranchWhitespaceRemoval = T.filter (\x -> (not . (elem x)) (" *" :: [Char]))
        branches = Set.fromList $ map ("heads/" +@)
               $ containedInBranches & gitBranchWhitespaceRemoval & T.lines
        matchingRefs = filter (`Set.member` branches) sortedRefs
        matchingBranches = map (T.drop (T.length "heads/")) $ matchingRefs

    return matchingBranches

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

data MailInfo = MailInfo {
        miMail    :: Mail
      , miSubject :: SubjectLine
    }

data CommitContentInfo = CommitContentInfo {
        cciInexactDiffHash    :: InexactDiffHash
      , cciInexactDiffHashNew :: Bool
      , cciFormatted          :: F.FList
      , cciMail               :: MailInfo
    }

data CommitInfo = CommitInfo {
        ciAuthorName         :: Text
      , ciCommitSubject      :: Text
      , ciContent            :: Either String CommitContentInfo
    }

ciToMaybeMailInfo :: CommitInfo -> Maybe MailInfo
ciToMaybeMailInfo (CommitInfo _ _ (Right (CommitContentInfo _ _ _ mi))) = Just mi
ciToMaybeMailInfo _ = Nothing

highlightSourceInDiffFile :: (MonadGitomail m) =>
              Text -> Text -> DH.DiffHeader -> DH.DiffContent -> m (Maybe F.FList)
highlightSourceInDiffFile fromBlobHash toBlobHash diffMeta content  = do
    fromFilenameI <- newIORef Nothing
    toFilenameI <- newIORef Nothing

    forM_ diffMeta $ \(_, f) ->
        case f of
            F.DiffRemoveFile fromFilename ->
                writeIORef fromFilenameI $ Just fromFilename
            F.DiffAddFile toFilename ->
                writeIORef toFilenameI $ Just toFilename
            _ -> return ()

    fromFilenameM <- readIORef fromFilenameI
    toFilenameM <- readIORef toFilenameI

    case (fromFilenameM, toFilenameM) of
        (Just fromFilename, Just toFilename) -> do
            repoPath <- getRepositoryPath
            let readMaybeBlob hash =
                    case hash of
                        "0000000000000000000000000000000000000000" -> return ""
                        _ -> GIT.readBlob repoPath hash

            fromB <- readMaybeBlob fromBlobHash
            toB <- readMaybeBlob toBlobHash

            let highlightWholeBlob filename blob =
                    F.splitToLinesArray $
                           (getHighlighter filename) (T.decodeUtf8 blob)
                fromHighlighted = highlightWholeBlob fromFilename fromB
                toHighlighted = highlightWholeBlob toFilename toB

            let _dumpHighlight h =
                  forM_ (zip [1 :: Int ..] (A.elems h)) $ \(idx, line) ->
                      liftIO $ T.putStrLn $
                           T.concat [(T.pack $ show idx), ": ", F.flistToText line]

            -- _dumpHighlight fromHighlighted

            hunkIndexesI <- newIORef Nothing
            content' <- dlistForM content $ \x ->
                let (def, _) = x
                    keepIt = return $ F.fragmentize [(def, Nothing)]
                    takeLine src prepText a1 b1 f = do
                        mIndexes <- readIORef hunkIndexesI
                        case mIndexes of
                            Just (fromIdx, toIdx) -> do
                                writeIORef hunkIndexesI (Just (fromIdx + a1, toIdx + b1))
                                -- TODO: idx error handling
                                let r = F.TPlain prepText `DList.cons` (src A.! f (fromIdx, toIdx))
                                    _dumpAnsi = liftIO $ T.putStr $ FA.ansiFormatting $ r
                                return r

                            Nothing -> keepIt

                in case x of
                (t, F.DiffHunkHeader) -> do
                    let r = "^@@ -([0-9]+),[0-9]+ [+]([0-9]+),[0-9]+ @@" :: Text
                    case (t =~ r) :: [[Text]] of
                        [[_, startFromStr, startToStr]] -> do
                            -- TODO: error handling
                            writeIORef hunkIndexesI
                                (Just ((read $ T.unpack startFromStr) :: Int,
                                       (read $ T.unpack startToStr)   :: Int))
                        _ -> writeIORef hunkIndexesI Nothing
                    keepIt

                (_, F.DiffUnchanged) -> takeLine fromHighlighted " " 1 1 fst
                (_, F.DiffRemove)    -> takeLine fromHighlighted "-" 1 0 fst
                (_, F.DiffAdd)       -> takeLine toHighlighted   "+" 0 1 snd

                _ -> keepIt

            return $ Just ((F.clearFormatting diffMeta) `DList.append` dlistConcat content')
        _ -> return Nothing

highlightSourceInDiff :: (MonadGitomail m) => DH.ParsedDiff -> m F.FList
highlightSourceInDiff parsed = do
    fmap dlistConcat $ dlistForM parsed $ \case
        Left other -> return $ F.clearFormatting other
        Right (diffMeta, content) -> do
            let def = F.clearFormatting diffMeta `DList.append` F.clearFormatting content
                r = "^index ([a-f0-9]+)[.][.]([a-f0-9]+)( .*)?\n$" :: Text
                indexFind (x, _) = "index " `T.isPrefixOf` x

            case filter indexFind diffMeta of
                [(line, _)] ->
                    case ((line :: Text) =~+ r) :: [[Text]] of
                        [[_, a, b, c]] -> do
                            let modDiffMeta =
                                    map (\x -> if indexFind x
                                                  then (T.concat [
                                                      "index ", T.take 12 a,
                                                      "..", T.take 12 b, c, "\n"], snd x)
                                                  else x) diffMeta
                            x <- highlightSourceInDiffFile a b modDiffMeta content
                            return $ fromMaybe def x
                        _ -> return def
                _ -> return def

data CommitMailKind = CommitMailSummary | CommitMailFull
    deriving Eq

getCommitInfo :: (MonadGitomail m)
                     => CommitMailKind
                     -> DB
                     -> O.GitRef
                     -> GIT.GitCommitHash
                     -> Maybe Int
                     -> m CommitInfo
getCommitInfo cmk db ref commitHash maybeNr = do
    opts <- gets opts
    when (opts ^. O.verbose) $ do
        putStrLn $ "makeOne: ref: " ++ show ref ++ " hash: " ++ show commitHash

    (authorName, commitSubjectLine, authorEMail, parentHashesStr) <- do
        let keys = intersperse commitHash
                       ["%aN", "%s", "%ae", "%P"]
        commitData <- fmap removeTrailingNewLine $ gitCmd ["show", commitHash, T.concat $ "--pretty=":keys, "-s"]
        case T.splitOn commitHash commitData of
            [a,b,c,d] -> return (a,b,c,d)
            _ -> E.throw $ InvalidCommandOutput "TODO"

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
            matched <- matchFiles commitHash

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

                      let htmlOnlyHeader = T.concat $ (intersperse "<br>" extraInfo) ++ ["<br>"]
                          extraInfo = catMaybes [
                                  commitURL
                                , (Just $ T.concat $ [ "Branches: ",
                                              T.concat (intersperse ", " containedInBranchesList)])
                                , flagsMaybe
                              ]

                      parsedFLists <- case cmk of
                          CommitMailFull -> do
                              diffHighlighted <-
                                  case config ^.|| CFG.sourceHighlight of
                                      False -> return $ DH.highlight diff
                                      True -> do  sourceInDiffHighlighted <- highlightSourceInDiff (DH.parseDiff diff)
                                                  let text = F.flistToText sourceInDiffHighlighted
                                                      diffHighlighted = DH.highlight text
                                                  case F.combineFLists text diffHighlighted sourceInDiffHighlighted of
                                                      Left str -> do
                                                          -- ToDo: this error should be emitted.
                                                          liftIO $ T.putStrLn $ T.pack str
                                                          return diffHighlighted
                                                      Right x -> do
                                                          return x

                              return $ (F.highlightMonospace commitMessageBody)
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
                          flists = parsedFLists `DList.append` emailFooter
                          html = TL.fromChunks [ htmlOnlyHeader, F.flistToInlineStyleHtml blobInCommitURLFunc flists ]
                          plain = TL.fromChunks [ F.flistToText flists ]
                          replyTo = Address (Just authorName) authorEMail
                          mailInfo = MailInfo mail subjectLine
                          contentInfo = CommitContentInfo diffInexactHash miInexactDiffHashNew flists mailInfo

                      returnCommitInfo $ Right contentInfo


sendMails :: (MonadGitomail m) => [(IO (), MailInfo)] -> m ()
sendMails mails = do
    if length mails == 0
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
                e mconn (opts, indexI) ((MailInfo {..})) = do
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
                        liftIO $ T.putStr $ F.fshow cciFormatted
                    liftIO $ T.putStr $ FA.ansiFormatting $ cciFormatted
                    return ()
