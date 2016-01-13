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

module Gitomail.Automailer
    ( autoMailer
    , autoMailerSetRef
    , showAutoMailerRefs
    , forgetHash
    , UnexpectedGitState(..)
    ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted   as E
import           Control.Lens.Operators     ((&), (^.))
import           Control.Monad              (forM, forM_, when)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State.Strict (gets)
import qualified Data.ByteString.Char8      as BS8
import qualified Data.DList                 as DList
import           Data.Foldable              (toList)
import           Data.List                  (nub, (\\))
import qualified Data.Map                   as Map
import           Data.Maybe                 (catMaybes, fromMaybe)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as TL
import           Data.Typeable              (Typeable)
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
import           Lib.Monad                  (whenM)
import           Lib.Text                   ((+@))
import           Lib.LiftedPrelude
------------------------------------------------------------------------------------

data UnexpectedGitState = UnexpectedGitState String deriving (Typeable)
instance E.Exception UnexpectedGitState
instance Show UnexpectedGitState where
    show (UnexpectedGitState msgstr) = "UnexpectedGitState: " ++ msgstr

data CommitSummaryType
    = CSTBranchPoint O.GitRef
    | CSTNew
    | CSTExisting
      deriving Show

data SummaryInfo = SummaryInfo
      { siCommitHash :: !GIT.GitCommitHash
      , siType       :: !CommitSummaryType
      } deriving Show

data RefMod
    = NewRef
    | ModifiedRef GIT.GitCommitHash
      deriving Show

data RefFFMod
    = RefFFModFast
    | RefFFModNoFast
      deriving Show

data BranchDetails
    = BranchAddedCommits [GIT.GitOid] [GIT.GitOid]
    | BranchRebased      [GIT.GitOid] [GIT.GitOid]
    | BranchNew          [GIT.GitOid]
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

refsMapDBKey :: BS8.ByteString
refsMapDBKey = "refs-map"

readRefsMap :: (MonadIO m) => DB -> m (Maybe (Map.Map O.GitRef GIT.GitCommitHash))
readRefsMap db = do
    -- FIXME: sensitive to how hackage git does 'read' / 'show'. We should newtype.
    mRefs <- DB.get db DB.defaultReadOptions refsMapDBKey
    case mRefs of
        Nothing -> return Nothing
        Just oldRefs ->
            return $ Just ((read . BS8.unpack) oldRefs)

writeRefsMap :: (MonadIO m) => O.Opts -> DB -> (Map.Map O.GitRef GIT.GitCommitHash) -> m ()
writeRefsMap opts db refMap = do
    when (not (opts ^. O.dryRun)) $ do
        DB.put db (DB.defaultWriteOptions {DB.sync = True })
            refsMapDBKey (BS8.pack $ show refMap)

autoMailerSetRef :: MonadGitomail m => O.GitRef -> GIT.GitCommitHash -> m ()
autoMailerSetRef gitref hash = do
    opts <- gets opts
    withDB $ \db -> do
        mRefs <- readRefsMap db
        case mRefs of
            Nothing -> return ()
            Just refs -> do
                let prev = (Map.lookup gitref refs)
                putStrLn $ "Current map: " ++ show refs
                putStrLn $ "Prev value: " ++ show prev
                case hash of
                    "-" -> return ()
                    _ -> do
                        let modified = Map.insert gitref hash refs
                        putStrLn $ "Now set to value: " ++ show hash
                        writeRefsMap opts db modified

makeSummaryEMail :: (MonadGitomail m)
                  => DB
                  -> (Text, GIT.GitCommitHash)
                  -> RefFFMod
                  -> RefMod
                  -> [SummaryInfo]
                  -> m (Map.Map GIT.GitCommitHash Int, Either String MailInfo)
makeSummaryEMail db (ref, topCommit) refMod isNewRef commits = do
    config <- getConfig
    case length commits >= 1 of
        True -> do
            commitsSeenI <- newIORef Set.empty
            newCommitsI <- newIORef []
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
                       CSTNew           -> do nr <- getNr
                                              modifyIORef' newCommitsI ((Nothing, Just nr, siCommitHash) :)

            newCommits <- readIORef newCommitsI
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
                        ("Content"          , newCommits,       True)
                      , ("Previously pushed", belowOrEqOldRef,  True)
                      , ("Branch points"    , branchPoints,     False)
                      ]
                let insert sI x = modifyIORef' sI (`DList.snoc` x)

                flistI <- newIORef DList.empty
                mailsI <- newIORef DList.empty

                forM_ commitLists $ \(name, list, includeCCTo) -> do
                   emptySoFarI <- newIORef True
                   forM_ list $ \(maybeRefName, maybeNr, commitHash) -> do
                       mailinfo <- makeOneMailCommit CommitMailSummary db ref commitHash maybeNr
                       case mailinfo of
                           Right (MailInfo{..}, CommitInfo{..}) -> do
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
                               insert mailsI $ if includeCCTo
                                                    then miMail
                                                    else miMail { mailCc = [], mailTo = [] }
                               linkToWeb <- getCommitURL mappedCommitHash >>= \case
                                   Nothing  -> return id
                                   Just url -> return $ \x -> F.mkFormS (F.Link url) x

                               insert flistRowI $ F.TForm (F.TableCellPad 10) (F.mkPlain "")

                               insert flistRowI $ F.TForm col $ F.mkPlain $ ciAuthorName +@ " "
                               insert flistRowI $ F.TForm col $ F.mkFormS F.Monospace $ linkToWeb $ F.mkPlain $ githash +@ " "

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

                               let maybeBold f =
                                       if ciInexactDiffHashNew then F.mkFormS F.Emphesis f else f
                               insert flistRowI $ F.TForm col $ maybeBold $ F.mkPlain field
                               insert flistRowI $ F.TForm col $ maybeBold $ F.mkPlain $ ciCommitSubject +@ "\n"

                               flistRow <- readIORef flistRowI
                               insert flistI $ F.TForm row flistRow
                           Left _ ->
                               return ()

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
                        html        = TL.fromChunks [ F.flistToInlineStyleHtml Nothing flist ]
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
    refsByPriority <- getSortedRefs
    opts <- gets opts

    let logDebug = when (opts ^. O.verbose)
        refsMap = Map.fromList $ concat refsByPriority

    withDB $ \db -> do
        mOldRefs <- readRefsMap db
        let initTracking = (mOldRefs == Nothing)
        when initTracking $ do
            putStrLn "Initial save of ref map. Will start tracking refs from now (this may take awhile)"

        let oldRefsMap = fromMaybe refsMap mOldRefs
        updatedRefsI <- newIORef oldRefsMap

        let putDB opt k v =
                when (not (opts ^. O.dryRun)) $ do
                    DB.put db opt k v
            syncOp = True
            asyncOp = False
            markSeen sync siCommitHash =
                putDB (DB.defaultWriteOptions {DB.sync = sync })
                   (commitSeenKey (T.encodeUtf8 siCommitHash))
                   "true"
            updateRefsMap = do
                x <- readIORef updatedRefsI
                writeRefsMap opts db x

        refStat <- fmap concat $ forM refsByPriority $ \refList -> do
            forM refList $ \(refname, commitHash) -> do
                case Map.lookup refname oldRefsMap of
                    Nothing -> return $ (refname, commitHash, Just $ NewRef)
                    Just oldCommitHash -> do -- Changed branch
                        if oldCommitHash /= commitHash
                          then return $ (refname, commitHash, Just $ ModifiedRef oldCommitHash)
                          else return $ (refname, commitHash, Nothing)

        worldI <- newIORef (Map.empty :: Map.Map GIT.GitOid (O.GitRef, [GIT.GitOid]))

        putStrLn "Relating commits to refs"

        refCommits <- fmap catMaybes $ do
            forM refStat $ \(refname, topCommitHash, maybeRefInRepo) -> do
                logDebug $ putStrLn $ "ref: " ++ T.unpack refname ++ " "
                              ++ T.unpack topCommitHash ++ " " ++ show maybeRefInRepo
                branchPointsI <- newIORef Set.empty
                let iterF world startCommitHash checkBranchPoints = do
                      seenI <- newIORef Map.empty
                      let iter = GIT.iterateHistoryUntil () $ \() commit parents -> do
                             seen <- readIORef seenI
                             if not (commit `Map.member` world) && not (commit `Map.member` seen)
                                 then do writeIORef seenI $ Map.insert commit (refname, parents) seen
                                         when initTracking $ markSeen asyncOp $ GIT.oidToText commit
                                         return ((), parents)
                                 else do when checkBranchPoints $
                                             modifyIORef' branchPointsI (Set.insert commit)
                                         return ((), [])
                      _ <- iter repoPath startCommitHash
                      readIORef seenI

                world <- readIORef worldI
                cur <- iterF world topCommitHash True
                result <- case maybeRefInRepo of
                    Just r -> do
                        branchPoints <- readIORef branchPointsI
                        let refInfo = (refname, r, topCommitHash, branchPoints)
                         in case r of
                              NewRef -> return $ Just (refInfo, Map.keys $ cur, Nothing)
                              (ModifiedRef oldHash) -> do
                                  old <- iterF world oldHash False
                                  return $ Just (refInfo, Map.keys $ cur, Just (oldHash, Map.keys $ old))
                    Nothing ->
                        return Nothing
                writeIORef worldI $ Map.union world cur
                return result

        let commitSet startList = do
                listI <- newIORef (startList :: [GIT.GitOid])
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
                        set <- readIORef setI
                        setTemp <- readIORef setTempI
                        when (not (n `Set.member` set) && not (n `Set.member` setTemp)) $ do
                            modifyIORef' setTempI $ Set.insert n

                            world <- readIORef worldI
                            case Map.lookup n world of
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
                    oid' <- liftIO $ GIT.textToOid oldHash
                    let added = osFromCur `osDiff` osFromOld
                        (commitSetFromCur, _) = osFromCur
                        isFast = oid' `Set.member` commitSetFromCur
                    case isFast of
                        True -> do
                            let existing = osFromOld `osIntersection` commitsFromOld
                            return $ (refInfo, BranchAddedCommits (snd added) (snd existing))
                        False -> do
                            let removed = osFromOld `osDiff` osFromCur
                            return $ (refInfo, BranchRebased (snd added) (snd removed))
                Nothing -> do
                    let content = osFromCur `osIntersection` commitsFromCur
                    return $ (refInfo, BranchNew (snd content))

        mailsI <- newIORef []
        markedForSendingI <- newIORef Set.empty

        when (not initTracking) $ do

            forM_ branchChanges $ \(refInfo, branchChange) -> do
                let (refName, refInRepo, topCommitHash, branchPoints) = refInfo
                liftIO $T.putStrLn $ "Ref " +@ refName +@ ": creating summaries"

                world <- readIORef worldI
                let f refmod commits = do
                        (numbersMap, summaryMailInfo) <- makeSummaryEMail db (refName, topCommitHash) refmod refInRepo
                            (commitsBranchPoints ++ commits)
                        return (numbersMap, summaryMailInfo, commits)
                    ourCommmits oid =
                        case Map.lookup oid world of
                            Just (otherRefName, _) -> otherRefName == refName
                            Nothing -> False
                    roots oid =
                        case Map.lookup oid world of
                            Just (_, []) -> Just oid
                            _ -> Nothing
                    noRoots oids = catMaybes $ map roots oids
                    commitsBranchPoints =
                           catMaybes $ map (\oid -> g oid (Map.lookup oid world)) (Set.toList branchPoints)
                        where g oid (Just (ref, _)) = if ref /= refName
                                                          then Just $ SummaryInfo (GIT.oidToText oid) (CSTBranchPoint ref)
                                                          else Nothing
                              g _    Nothing        = Nothing

                (numbersMap, summaryMailInfo, commits) <- case branchChange of
                    BranchAddedCommits added existing -> do
                        logDebug $ putStrLn $ "ref: added - " ++ T.unpack refName ++ " "
                            ++ (show $ length added) ++ " commits, " ++ (show $ length existing) ++ " existing"
                        let ourExisting = reverse $ filter ourCommmits existing
                            commitsBefore = case noRoots ourExisting of
                                                [] -> map (\oid -> SummaryInfo (GIT.oidToText oid) CSTExisting) ourExisting
                                                (_:_) -> []
                            commitsAfter = reverse $ map (\oid -> SummaryInfo (GIT.oidToText oid) CSTNew) $
                                             filter ourCommmits added
                        f RefFFModFast $ commitsBefore ++ commitsAfter
                    BranchRebased added removed -> do
                        logDebug $ putStrLn $ "ref: rebase - " ++ T.unpack refName ++ " "
                            ++ (show $ length added) ++ " commits, " ++ (show $ length removed) ++ " removed"
                        let commitsAfter = reverse $ map (\oid -> SummaryInfo (GIT.oidToText oid) CSTNew) $
                                             filter ourCommmits added
                        f RefFFModNoFast $ commitsAfter
                    BranchNew content -> do
                        logDebug $ putStrLn $ "ref: new - " ++ T.unpack refName ++ " "
                            ++ (show $ length content) ++ " commits, "
                        let commits = reverse $ map (\oid -> SummaryInfo (GIT.oidToText oid) CSTNew) content
                        f RefFFModFast $ commits

                modifyIORef' mailsI ((:) (return (), summaryMailInfo))

                liftIO $ T.putStrLn $ "Ref " +@ refName +@ ": creating E-Mails"

                forM_ commits $ \SummaryInfo {..} -> do
                    isNew <- commitHashIsNew db siCommitHash
                    notSent <- markInIORefSet markedForSendingI siCommitHash
                    shownCommitHash <- mapCommitHash siCommitHash
                    if | isNew && notSent
                                -> do putStrLn $ "  Formatting " ++ (T.unpack shownCommitHash)
                                      info <- makeOneMailCommit CommitMailFull
                                            db refName siCommitHash (Map.lookup siCommitHash numbersMap)
                                      let mailinfoAndAction = (action, fmap fst info)
                                          action = do
                                              markSeen syncOp siCommitHash
                                              modifyIORef' updatedRefsI $ Map.insert refName siCommitHash
                                              updateRefsMap
                                              case info of
                                                  Right (MailInfo {..}, CommitInfo{..}) ->
                                                      putInexactDiffHashInDB db ciInexactDiffHash
                                                  Left _ -> return ()
                                      modifyIORef' mailsI ((:) mailinfoAndAction)
                       | otherwise -> putStrLn $ "  Skipping old commit " ++ (T.unpack shownCommitHash)


            mails <- fmap reverse $ readIORef mailsI
            when (length mails /= 0) $ do
                putStrLn $ "Sending all E-Mails"
            sendMails mails

        writeIORef updatedRefsI refsMap

        finalRefs <- readIORef updatedRefsI
        when (Just finalRefs /= mOldRefs || initTracking) $ do
            writeRefsMap opts db finalRefs

showAutoMailerRefs :: (MonadGitomail m) => m ()
showAutoMailerRefs = do
    refs <- getSortedRefs
    forM_ refs $ \lst -> do
        putStrLn "--"
        forM_ lst $ \(ref, hash) -> do
            putStrLn $ concat $ [T.unpack hash, " ", T.unpack ref]
