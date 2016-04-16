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
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Gitomail.Automailer
    ( autoMailer
    , showAutoMailerRefs
    , forgetHash
    , UnexpectedGitState(..)
    ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted   as E
import           Control.Lens.Operators     ((&), (^.))
import           Control.Monad              (forM, forM_, when, void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State.Strict (gets)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.DList                 as DList
import           Data.Foldable              (toList)
import qualified Data.HashMap.Strict        as HMS
import           Data.List                  (nub, (\\))
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as TL
import           Database.LevelDB.Base      (DB)
import qualified Database.LevelDB.Base      as DB
import           Network.Mail.Mime          (Mail (..), htmlPart, plainPart)
import           Text.Regex.TDFA.Text       ()
----
import           Gitomail.CommitToMail
import           Gitomail.Config            ((^.||))
import qualified Gitomail.Config            as CFG
import           Gitomail.Gitomail
import qualified Gitomail.Opts              as O
import qualified Lib.Formatting             as F
import qualified Lib.Git                    as GIT
import qualified Lib.InlineFormatting       as F
import qualified Lib.Map                    as Map
import           Lib.LiftedPrelude
import           Lib.Monad                  (whenM)
import           Lib.Text                   ((+@))
------------------------------------------------------------------------------------

data UnexpectedGitState = UnexpectedGitState String
instance E.Exception UnexpectedGitState
instance Show UnexpectedGitState where
    show (UnexpectedGitState msgstr) = "UnexpectedGitState: " ++ msgstr

data CommitSummaryType
    = CSTBranchPoint O.GitRef
    | CSTNew
    | CSTRemoved
    | CSTExisting
      deriving Show

data SummaryInfo = SummaryInfo
      { siCommitHash :: !GIT.CommitHash
      , siType       :: !CommitSummaryType
      } deriving Show

data RefFFMod
    = RefFFModFast
    | RefFFModNoFast
      deriving Show

data BranchDetails
    = BranchAddedCommits [GIT.CommitHash] [GIT.CommitHash]
    | BranchRebased      [GIT.CommitHash] [GIT.CommitHash] [GIT.CommitHash]
    | BranchNew          [GIT.CommitHash]
      deriving Show

commitSeenKey :: BS8.ByteString -> BS8.ByteString
commitSeenKey commit = BS8.concat [ "commit-seen-",  commit]

commitHashIsNew :: (MonadIO m) => DB -> Text -> m Bool
commitHashIsNew db commitT = do
    v <- DB.get db DB.defaultReadOptions
         $ commitSeenKey $ T.encodeUtf8 commitT
    return $ maybe True (const False) v

markInIORefSet :: (Ord a, MonadIO m) =>
                  IORef (Set.Set a) -> a -> m Bool
markInIORefSet ioref item = do
    set <- readIORef ioref
    if not $ item `Set.member` set
       then do writeIORef ioref (item `Set.insert` set)
               return True
       else return False

forgetHash :: (MonadGitomail m) => m ()
forgetHash = do
    opts <- gets opts
    case opts ^. O.gitRef of
        Just hash -> do
            withDB $ \db -> do
                let k = commitSeenKey (T.encodeUtf8 hash)
                v <- DB.get db DB.defaultReadOptions k
                case v of
                    Nothing -> putStrLn "Hash not in DB"
                    Just _ -> do DB.delete db DB.defaultWriteOptions k
                                 putStrLn "Sucesss"
        Nothing -> putStrLn "Hash not specified"

addIssueTrackLinks :: (MonadGitomail m) => Text -> m F.FList
addIssueTrackLinks msg = parseIssueTrackMentions F.TPlain (\a b -> F.TForm (F.Link a) b) msg

makeSummaryEMail :: (MonadGitomail m)
                  => DB
                  -> (Text, GIT.CommitHash)
                  -> RefFFMod
                  -> RefMod
                  -> [SummaryInfo]
                  -> m (Map.Map GIT.CommitHash Int, Either String MailInfo)
makeSummaryEMail db (ref, topCommit) refMod isNewRef commits = do
    config <- getConfig
    case length commits >= 1 of
        True -> do
            commitsSeenI <- newIORef Set.empty
            newCommitsI <- newIORef []
            removedCommitsI <- newIORef []
            belowOrEqOldRefI <- newIORef []
            branchPointsI <- newIORef []
            numberingI <- newIORef (1 :: Int)

            forM_ commits $ \SummaryInfo {..} -> do
                commitsSeen <- readIORef commitsSeenI
                when (not $ siCommitHash `Set.member` commitsSeen) $ do
                   writeIORef commitsSeenI $ Set.insert siCommitHash commitsSeen
                   let getNr = do
                           n <- readIORef numberingI
                           writeIORef numberingI (n + 1)
                           return n
                   case siType of
                       CSTBranchPoint refName
                                        -> modifyIORef' branchPointsI ((Just refName, Nothing, siCommitHash) :)
                       CSTExisting      -> do nr <- getNr
                                              modifyIORef' belowOrEqOldRefI ((Nothing, Just nr, siCommitHash) :)
                       CSTRemoved       -> do modifyIORef' removedCommitsI ((Nothing, Nothing, siCommitHash) :)
                       CSTNew           -> do nr <- getNr
                                              modifyIORef' newCommitsI ((Nothing, Just nr, siCommitHash) :)

            newCommits <- readIORef newCommitsI
            removedCommits <- readIORef removedCommitsI
            belowOrEqOldRef <- readIORef belowOrEqOldRefI
            branchPoints <- readIORef branchPointsI

            subject <- do
                githashTo <- mapCommitHash topCommit >>= githashRepr
                let commitsStr 1 = "1 commit"
                    commitsStr n = T.concat [T.pack $ show n, " commits"]
                    commitsSeenStr = T.concat [commitsStr $
                                               (length belowOrEqOldRef) + (length newCommits)]
                    commitsNotReachedOld' = commitsStr $ length newCommits
                  in case (isNewRef, refMod) of
                     (NewRef, _)  -> do
                         return [GIT.refRepr ref, " is new with ",  commitsSeenStr]
                     (ModifiedRef hash, RefFFModFast)  -> do
                         githashFrom <- mapCommitHash hash >>= githashRepr
                         return [(GIT.refRepr ref), " added ",
                                 commitsNotReachedOld',
                                 if commitsSeenStr /= commitsNotReachedOld'
                                     then T.concat [ " (having ", commitsSeenStr, " total)" ]
                                     else "",
                                 ": ", githashFrom, "..", githashTo]
                     (ModifiedRef hash, RefFFModNoFast) -> do
                         githashFrom <- mapCommitHash hash >>= githashRepr
                         return [(GIT.refRepr ref), " *rebased* having ",
                                 commitsSeenStr, " total: ", githashFrom, "...", githashTo]

            githashtoNumberI <- newIORef Map.empty
            (flist, mails) <- do
                let commitLists = [
                        ("Content"          , newCommits,       True,  id)
                      , ("Removed content"  , removedCommits,   True,  F.mkFormS F.LineThrough)
                      , ("Previously pushed", belowOrEqOldRef,  True,  id)
                      , ("Branch points"    , branchPoints,     False, id)
                      ]
                let insert sI x = modifyIORef' sI (`DList.snoc` x)

                flistI <- newIORef DList.empty
                mailsI <- newIORef DList.empty

                forM_ commitLists $ \(name, list, includeCCTo, textMod) -> do
                   emptySoFarI <- newIORef True
                   forM_ list $ \(maybeRefName, maybeNr, commitHash) -> do
                       ci@CommitInfo{..} <- getCommitInfo CommitMailSummary db ref commitHash maybeNr
                       let row = F.TableRow
                       let col' c = F.TableCol c
                       let col = col' 1

                       flistRowI <- newIORef DList.empty

                       whenM (readIORef emptySoFarI) $ do
                           writeIORef emptySoFarI False
                           insert flistI $ F.TForm (F.TableRow)
                                         $ F.mkFormS (col' 5)
                                         $ F.mkFormS F.Underline
                                         $ F.mkPlain (T.concat ["\n", name, "\n"])

                       mappedCommitHash <- mapCommitHash commitHash
                       githash <- githashRepr mappedCommitHash
                       case ciToMaybeMailInfo ci of
                           Just MailInfo{..} -> do
                               insert mailsI $ if includeCCTo
                                                 then miMail
                                                 else miMail { mailCc = [], mailTo = [] }
                           Nothing -> return ()

                       linkToWeb <- getCommitURL mappedCommitHash >>= \case
                           Nothing  -> return id
                           Just url -> return $ \x -> F.mkFormS (F.Link url) x

                       insert flistRowI $ F.TForm (F.TableCellPad 10) (F.mkPlain "")

                       insert flistRowI $ F.TForm col $ F.mkPlain $ ciAuthorName +@ " "
                       insert flistRowI $ F.TForm col $ textMod $ F.mkFormS F.Monospace $ linkToWeb $ F.mkPlain $ githash +@ " "

                       field <- case maybeNr of
                           Just nr -> do
                               modifyIORef' githashtoNumberI (Map.insert commitHash nr)
                               return $ T.pack $ "#" ++ show nr ++ " "
                           Nothing ->
                               case maybeRefName of
                                   Just refName -> do
                                       return $ "(" +@ GIT.refRepr refName +@ ") "
                                   Nothing ->
                                       return " "

                       let bold =
                               case ciContent of
                                   Left _ -> False
                                   Right CommitContentInfo{..} ->
                                       cciInexactDiffHashNew
                           maybeBold f =
                               if bold then F.mkFormS F.Emphesis f else f
                       insert flistRowI $ F.TForm col $ maybeBold $ F.mkPlain field
                       commitSubject <- addIssueTrackLinks $ ciCommitSubject +@ "\n"
                       insert flistRowI $ F.TForm col $ textMod $ maybeBold $ commitSubject

                       flistRow <- readIORef flistRowI
                       insert flistI $ F.TForm row flistRow

                insert flistI (F.TPlain "\n")

                flist <- readIORef flistI
                mails <- readIORef mailsI
                emailFooter <- getFooter
                return (F.mkFormS F.Table flist `DList.append` emailFooter, toList $ mails)

            (cc, to) <- getExtraCCTo

            emailAddress <- getFromEMail
            let toAddresses = nub $ to ++ (concat $ map mailTo mails)
                ccAddresses = nub $ cc ++ (concat $ map mailCc mails)
                (toAddresses', ccAddresses') =
                    if toAddresses == [] then (ccAddresses, [])
                                        else (toAddresses, ccAddresses)

            repoName <- getRepoName
            githashToNumber <- readIORef githashtoNumberI
            extraHeaders <- genExtraEMailHeaders emailAddress

            case toAddresses of
                [] -> return (githashToNumber, Left "No E-Mail destination for summary")
                _ -> do
                    let subjectLine =
                            config ^.|| CFG.summarySubjectLine
                            & T.replace "%r" repoName
                            & T.replace "%b" ref
                            & (T.replace "%s" $ T.concat subject)
                        html        = TL.fromChunks [ F.flistToInlineStyleHtml flist ]
                        plain       = TL.fromChunks [ F.flistToText flist ]
                        mail        = Mail
                          { mailFrom    = emailAddress
                          , mailTo      = toAddresses'
                          , mailCc      = ccAddresses' \\ toAddresses'
                          , mailBcc     = []
                          , mailHeaders = extraHeaders ++ [("Subject", subjectLine)]
                          , mailParts   = [[plainPart plain, htmlPart html]]
                          }
                    return (githashToNumber, Right $ MailInfo mail subjectLine)
        False ->
            return $ (Map.empty, Left "No commits, not sending anything")

autoMailer :: (MonadGitomail m) => m ()
autoMailer = do
    repoPath <- getRepositoryPath
    (refsByPriority, prevRefs) <- getSortedRefs
    opts <- gets opts

    let logDebug = when (opts ^. O.verbose)
    let refsMap = Map.fromList $ concat refsByPriority
    let prevRefsMap = Map.fromList prevRefs

    withDB $ \db -> do
        (initTracking, oldRefsMap) <- if (prevRefs == [])
            then do putStrLn "Initial save of ref map. Will start tracking refs from now (this may take awhile)"
                    return (True, refsMap)
            else return (False, prevRefsMap)

        let putDB opt k v =
                when (not (opts ^. O.dryRun)) $ do
                    DB.put db opt k v
        let syncOp = True
        let asyncOp = False
        let refNamePrefixed refName = T.concat ["refs/gitomail/", refName]
        let updateRefCmd (refName, siCommitHash) =
                gitCmdIO repoPath ["update-ref", refNamePrefixed refName, siCommitHash]
        let removeRefCmd refName =
                gitCmdIO repoPath ["update-ref", "-d", refNamePrefixed refName]
        let rememberChangedRefs = do
                let md = Map.describeDifferences prevRefsMap refsMap
                forM_ (Map.toList $ Map.newValues md) updateRefCmd
                forM_ (Map.keys $ Map.inFirst md) removeRefCmd
        let markSeen sync siCommitHash =
                putDB (DB.defaultWriteOptions {DB.sync = sync })
                   (commitSeenKey (T.encodeUtf8 siCommitHash))
                   "true"

        putStrLn "Relating commits to refs"

        (refCommits, world) <- relateCommits refsByPriority oldRefsMap
        when initTracking $ do
            forM_ (HMS.keys world) $ markSeen asyncOp

        let commitSet startList = do
                listI <- newIORef (startList :: [GIT.CommitHash])
                resListI <- newIORef []
                setI <- newIORef $ Set.empty
                setTempI <- newIORef $ Set.empty
                let loop = do
                        list <- readIORef listI
                        case list of
                            (x:xs) -> do
                                writeIORef listI xs
                                visit x
                                loop
                            [] -> return ()
                    visit n = do -- Tarjan's algorithm
                        b <- do
                            set <- readIORef setI
                            setTemp <- readIORef setTempI
                            return $ (not (n `Set.member` set) && not (n `Set.member` setTemp))
                        when b $ do
                            modifyIORef' setTempI $ Set.insert n

                            case HMS.lookup n world of
                                Just (_, parents) -> forM_ parents visit
                                Nothing -> return ()

                            modifyIORef' setI $ Set.insert n
                            modifyIORef' setTempI $ Set.delete n
                            modifyIORef' resListI ((:) n)

                loop
                set <- readIORef setI
                list <- readIORef resListI
                return (set, list)

        branchChanges <- do
          forM refCommits $ \(refInfo, commitsFromCur, maybeCommitsfromOld) -> do
            osFromCur <- commitSet commitsFromCur
            let orderSetOp op (_, l1) (_, l2) =
                    let m1 = Map.fromList $ zip l1 [(0 :: Int)..]
                        m2 = m1 `op` Map.fromList (zip l2 [(0 :: Int)..])
                     in (Set.fromList $ Map.keys m2,
                         Map.elems $ Map.fromList $ map (\(x, y) -> (y, x)) $ Map.toList m2)
                osDiff = orderSetOp Map.difference
                osIntersection os1 l2 = orderSetOp Map.intersection os1 ((), l2)

            case maybeCommitsfromOld of
                Just (oldHash, commitsFromOld) -> do
                    osFromOld <- commitSet commitsFromOld
                    let added = osFromCur `osDiff` osFromOld
                        (commitSetFromCur, _) = osFromCur
                        isFast = oldHash `Set.member` commitSetFromCur
                        existing = osFromOld `osIntersection` commitsFromOld
                    case isFast of
                        True -> do
                            return $ (refInfo, BranchAddedCommits (snd added) (snd existing))
                        False -> do
                            let removed = osFromOld `osDiff` osFromCur
                            return $ (refInfo, BranchRebased (snd added) (snd removed) (snd existing))
                Nothing -> do
                    let content = osFromCur `osIntersection` commitsFromCur
                    return $ (refInfo, BranchNew (snd content))

        let
          normalOperation = do
            mailsI <- newIORef []
            markedForSendingI <- newIORef Set.empty

            forM_ branchChanges $ \(refInfo, branchChange) -> do
                let (refName, refInRepo, topCommitHash, branchPoints) = refInfo
                liftIO $T.putStrLn $ "Ref " +@ refName +@ ": creating summaries"

                let f refmod commits = do
                        (numbersMap, summaryMailInfo) <- makeSummaryEMail db (refName, topCommitHash) refmod refInRepo
                            (commitsBranchPoints ++ commits)
                        return (numbersMap, summaryMailInfo, commits)
                    ourCommit oid =
                        case HMS.lookup oid world of
                            Just (otherRefName, _) -> otherRefName == refName
                            Nothing -> False
                    roots oid =
                        case HMS.lookup oid world of
                            Just (_, []) -> Just oid
                            _ -> Nothing
                    noRoots oids = catMaybes $ map roots oids

                    commitsBranchPoints =
                           catMaybes $ map (\oid -> g oid (HMS.lookup oid world)) (Set.toList branchPoints)
                        where g oid (Just (ref, _)) = if ref /= refName
                                                          then Just $ SummaryInfo oid (CSTBranchPoint ref)
                                                          else Nothing
                              g _    Nothing        = Nothing
                    commitsExisting existing =
                       let ourExisting = reverse $ filter ourCommit existing
                        in case noRoots ourExisting of
                                      [] -> map (\oid -> SummaryInfo oid CSTExisting) ourExisting
                                      (_:_) -> []
                    commitsRemoved removed = reverse $ map (\oid -> SummaryInfo oid CSTRemoved) $ removed
                    commitsNew added = reverse $ map (\oid -> SummaryInfo oid CSTNew) $
                                             filter ourCommit added

                (numbersMap, summaryMailInfo, commits) <- case branchChange of
                    BranchAddedCommits added existing -> do
                        logDebug $ putStrLn $ "ref: added - " ++ T.unpack refName ++ " "
                            ++ (show $ length added) ++ " commits, " ++ (show $ length existing) ++ " existing"
                        f RefFFModFast $ commitsExisting existing ++ commitsNew added
                    BranchRebased added removed existing -> do
                        logDebug $ putStrLn $ "ref: rebase - " ++ T.unpack refName ++ " "
                            ++ (show $ length added) ++ " commits, " ++ (show $ length removed) ++ " removed"
                        let filteredExisting = filter ourCommit existing
                            filteredAdded = filter ourCommit added
                            commits =
                                case (filteredExisting, filteredAdded) of
                                    (_, []) -> []
                                    ([], _) -> commitsNew added
                                    _  -> commitsExisting filteredExisting
                                             ++ commitsRemoved removed ++ commitsNew added
                        f RefFFModNoFast $ commits
                    BranchNew content -> do
                        logDebug $ putStrLn $ "ref: new - " ++ T.unpack refName ++ " "
                            ++ (show $ length content) ++ " commits, "
                        let commits = reverse $ map (\oid -> SummaryInfo oid CSTNew) content
                        f RefFFModFast $ commits

                case summaryMailInfo of
                    Right mailInfo -> modifyIORef' mailsI ((:) (return (), mailInfo))
                    Left _ -> return ()

                liftIO $ T.putStrLn $ "Ref " +@ refName +@ ": creating E-Mails"

                forM_ commits $ \SummaryInfo {..} -> do
                    isNew <- commitHashIsNew db siCommitHash
                    notSent <- markInIORefSet markedForSendingI siCommitHash
                    shownCommitHash <- mapCommitHash siCommitHash
                    if | isNew && notSent
                                -> do putStrLn $ "  Formatting " ++ (T.unpack shownCommitHash)
                                      info <- getCommitInfo CommitMailFull
                                            db refName siCommitHash (Map.lookup siCommitHash numbersMap)
                                      case info of
                                          CommitInfo _ _ (Right CommitContentInfo{..}) -> do
                                              let mailinfoAndAction = (action, cciMail)
                                                  action = do
                                                      markSeen syncOp siCommitHash
                                                      putInexactDiffHashInDB db cciInexactDiffHash
                                                      void $ updateRefCmd (refName, siCommitHash)
                                              modifyIORef' mailsI ((:) mailinfoAndAction)
                                          _ -> return ()
                       | otherwise -> putStrLn $ "  Skipping old commit " ++ (T.unpack shownCommitHash)


            mails <- fmap reverse $ readIORef mailsI
            when (length mails /= 0) $ do
                putStrLn $ "Sending all E-Mails"
            sendMails mails

        when (not initTracking) $
            normalOperation

        rememberChangedRefs

showAutoMailerRefs :: (MonadGitomail m) => m ()
showAutoMailerRefs = do
    (refs, prevRefMap) <- getSortedRefs
    let p lst = forM_ lst $ \(ref, hash) -> do
                   putStrLn $ concat $ [T.unpack hash, " ", T.unpack ref]
    forM_ refs $ \lst -> do
        putStrLn "--"
        p lst
    putStrLn "--"
    p prevRefMap
