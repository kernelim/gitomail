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
    , getAutoMailerRefs
    , showAutoMailerRefs
    , checkBranchPoints
    , forgetHash
    , UnexpectedGitState(..)
    ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted    as E
import           Control.Lens.Operators      ((^.), (&))
import           Control.Monad               (forM, forM_, when, filterM)
import           Control.Monad.IO.Class      (liftIO, MonadIO)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.DList                  as DList
import           Data.List                   (sortOn, groupBy, nub, intersperse,
                                              (\\))
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe, catMaybes)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import           Data.Foldable               (toList)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as TL
import           Data.Typeable               (Typeable)
import           Database.LevelDB.Base       (DB)
import qualified Database.LevelDB.Base       as DB
import           Control.Monad.State.Strict  (get, gets, lift)
import           Network.Mail.Mime           (Mail (..), htmlPart, plainPart)
import           Text.Regex.TDFA             ((=~))
import           Text.Regex.TDFA.Text        ()
----
import           Gitomail.Config             ((^.||))
import qualified Gitomail.Config             as CFG
import qualified Gitomail.Opts               as O
import           Gitomail.CommitToMail
import           Gitomail.Gitomail
import           Lib.Monad                   (whenM)
import           Lib.Text                    ((+@))
import           Lib.Process                 (ReadProcessFailed)
import qualified Lib.Git                     as GIT
import qualified Lib.Formatting              as F
import qualified Lib.InlineFormatting        as F
import           Lib.Regex                   (matchWhole)
import           Lib.Monad                   (lSeqForM)
import           Lib.LiftedPrelude
------------------------------------------------------------------------------------

data UnexpectedGitState = UnexpectedGitState String deriving (Typeable)
instance E.Exception UnexpectedGitState
instance Show UnexpectedGitState where
    show (UnexpectedGitState msgstr) = "UnexpectedGitState: " ++ msgstr

getRefScoreFunc :: (MonadGitomail m) => m (Text -> Int)
getRefScoreFunc = do
    config <- getConfig
    let rootRefsTexts = config ^.|| CFG.rootRefs
    let rootRefs = map matchWhole rootRefsTexts
    let scoreRef ref = fromMaybe 0 $ lookup True $ zip (rootRefs <*> [ref]) [1..(length rootRefsTexts)]
    return scoreRef

data RefState = NewRef | ModifiedRef GIT.GitCommitHash
data RefModState = RefModFastForward | RefModNoFastForward

data CommitIterationInfo = CommitIterationInfo
      { cCommitHash          :: !GIT.GitCommitHash
      , cNotReachedOldRef    :: !Bool
      , cIsBranchPoint       :: !Bool
      } deriving Show

makeSummaryEMail :: (MonadGitomail m)
                  => DB
                  -> (Text, GIT.GitCommitHash)
                  -> RefModState
                  -> RefState
                  -> [CommitIterationInfo]
                  -> [(GIT.GitOid, O.GitRef)]
                  -> m ((Map.Map GIT.GitCommitHash Int, Either String MailInfo))
makeSummaryEMail db (ref, topCommit) refMod isNewRef commits nonRootBranchPoints = do
    config <- getConfig
    case length commits >= 1 of
        True -> do
            commitsSeenI <- newIORef Set.empty
            newCommitsI <- newIORef []
            belowOrEqOldRefI <- newIORef []
            branchPointsI <- newIORef []
            numberingI <- newIORef (1 :: Int)

            forM_ commits $ \CommitIterationInfo {..} -> do
                commitsSeen <- readIORef commitsSeenI
                when (not $ cCommitHash `Set.member` commitsSeen) $ do
                   writeIORef commitsSeenI $ Set.insert cCommitHash commitsSeen
                   let getNr = do
                           n <- readIORef numberingI
                           writeIORef numberingI (n + 1)
                           return n
                   case (cIsBranchPoint, cNotReachedOldRef) of
                       (True, _)   -> modifyIORef' branchPointsI ((Nothing, cCommitHash) :)
                       (_, False)  -> do nr <- getNr
                                         modifyIORef' belowOrEqOldRefI ((Just nr, cCommitHash) :)
                       _           -> do nr <- getNr
                                         modifyIORef' newCommitsI ((Just nr, cCommitHash) :)

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

                    branchedFromStr [] = return ""
                    branchedFromStr (x:xs) = do
                        text <- branchedFromStr' (x:xs)
                        return $ T.concat [", from ", text]

                    branchedFromStr' xs =
                        fmap (T.concat . (intersperse ", ")) $ forM xs $ \(githash, ref') -> do
                            branchHashPref <-
                                mapCommitHash (GIT.oidToText githash) >>= githashRepr
                            return $ T.concat [GIT.refRepr ref', " @ ", branchHashPref]
                  in case (isNewRef, refMod) of
                     (NewRef, _)  -> do
                         branchStr <- branchedFromStr nonRootBranchPoints
                         return [GIT.refRepr ref, " is new with ",
                                 commitsSeenStr, branchStr]
                     (ModifiedRef hash, RefModFastForward)  -> do
                         githashFrom <- mapCommitHash hash >>= githashRepr
                         return [(GIT.refRepr ref), " added ",
                                 commitsNotReachedOld',
                                 if commitsSeenStr /= commitsNotReachedOld'
                                     then T.concat [ " (having ", commitsSeenStr, " total)" ]
                                     else "",
                                 ": ", githashFrom, "..", githashTo]
                     (ModifiedRef hash, RefModNoFastForward) -> do
                         branchStr <- branchedFromStr nonRootBranchPoints
                         githashFrom <- mapCommitHash hash >>= githashRepr
                         return [(GIT.refRepr ref), " *rebased* having ",
                                 commitsSeenStr, " total: ", githashFrom, "...", githashTo,
                                 branchStr]

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
                   forM_ list $ \(maybeNr, commitHash) -> do
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

                               nr <- case maybeNr of
                                   Just nr -> do
                                       modifyIORef' githashtoNumberI (Map.insert commitHash nr)
                                       return $ T.pack $ "#" ++ show nr ++ " "
                                   Nothing ->
                                       return " "

                               let maybeBold f =
                                       if ciInexactDiffHashNew then F.mkFormS F.Emphesis f else f
                               insert flistRowI $ F.TForm col $ maybeBold $ F.mkPlain nr
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

getAutoMailerRefs :: (MonadGitomail m) => m [[(O.GitRef, GIT.GitCommitHash)]]
getAutoMailerRefs = do
    refsMatcher <- getRefsMatcher
    refScore <- getRefScoreFunc
    let onlyRefs x = refsMatcher x

    refsLines <- fmap T.lines $ gitCmd ["show-ref", "--heads", "--tags", "--dereference"]
    byScores <- fmap catMaybes $ lSeqForM refsLines $ \line -> do
        let invalid f =
               E.throw $ InvalidCommandOutput ("git show-ref returned: " ++ show f)
        let pass hash refname = do
                return $
                    if onlyRefs refname
                          then Just (refScore refname, (refname, hash))
                          else Nothing
        case line =~ ("^([a-f0-9]+) refs/(tags/[^^]+)([\\^]{})?$" :: Text) of
            [[_, _, _, ""]] -> return Nothing
            [[_, hash, name, "^{}"]] -> pass hash name
            x1 -> case line =~ ("^([a-f0-9]+) refs/(heads/.*)$" :: Text) of
                [[_, hash, name]] -> pass hash name
                x2 -> invalid (line, x1, x2)
    let g = groupBy (\x y -> fst x == fst y) $ sortOn ((0 -) . fst) byScores
    return $ map (map snd) g

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
            withDB $ do
                db <- get
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
    withDB $ do
        db <- get
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

autoMailer :: (MonadGitomail m) => m ()
autoMailer = do
    refsByPriority <- getAutoMailerRefs
    let refsMap = Map.fromList $ concat refsByPriority

    repoPath <- getRepositoryPath
    (branchTop, _) <-
        liftIO $ GIT.analyseRefs repoPath refsByPriority >>= \case
            Left l -> E.throw $ UnexpectedGitState l
            Right r -> return r

    opts <- gets opts
    withDB $ do
        db <- get

        let putDB opt k v =
                when (not (opts ^. O.dryRun)) $ do
                    DB.put db opt k v

        mOldRefs <- readRefsMap db
        let initTracking = (mOldRefs == Nothing)
        when initTracking $ do
            putStrLn "Initial save of ref map. Will start tracking refs from now (this may take awhile)"

        let oldRefsMap = fromMaybe refsMap  mOldRefs

        newCommits <-
            fmap catMaybes $
            fmap concat $
            forM refsByPriority $ \lst ->
            forM lst $ \(ref, topCommit) ->
                case Map.lookup ref branchTop of
                Nothing -> do
                    putStrLn $ "WARNING: " ++ (show ref) ++ " has no branch point, this is odd"
                    return Nothing
                Just branchPoints -> do
                    when (opts ^. O.verbose) $ do
                        putStrLn $ "ref: " ++ (show ref) ++ ", top: " ++
                                   (show topCommit) ++ " old: " ++ (show (Map.lookup ref oldRefsMap))
                    fmap (fmap (\x -> ((ref, topCommit), x))) $ do
                        iterateRefCommits repoPath topCommit
                            (if initTracking then Nothing else Map.lookup ref oldRefsMap)
                            branchPoints

        mailsI <- newIORef []

        updatedRefsI <- newIORef oldRefsMap
        let updateRefsMap = do
                x <- readIORef updatedRefsI
                writeRefsMap opts db x

        alreadySentI <- newIORef Set.empty
        forM_ newCommits $ \((ref, topCommit), (refMod, nonRootBranchPoints,
                                                isNewRef, commitsinfo)) -> do
            let syncOp = False
                asyncOp = True
                markSeen sync cCommitHash =
                    putDB (DB.defaultWriteOptions {DB.sync = sync })
                       (commitSeenKey (T.encodeUtf8 cCommitHash))
                       "true"
            if initTracking
                then do
                    forM_ commitsinfo $ \CommitIterationInfo {..} -> do
                        markSeen asyncOp cCommitHash
                else do
                    putStrLn $ "Checking ref " ++ (T.unpack ref)
                                ++ ", " ++ (show $ length commitsinfo) ++ " commits"

                    (numbersMap, summaryMailInfo) <-
                        lift $ makeSummaryEMail db (ref, topCommit) refMod
                            isNewRef commitsinfo nonRootBranchPoints
                    modifyIORef' mailsI ((:) (return (), summaryMailInfo))

                    forM_ commitsinfo $ \CommitIterationInfo {..} -> do
                        isNew <- commitHashIsNew db cCommitHash
                        notSent <- markInIORefSet alreadySentI cCommitHash

                        shownCommitHash <- lift $ mapCommitHash cCommitHash
                        if | isNew && notSent
                                    -> do putStrLn $ "  Formatting " ++ (T.unpack shownCommitHash)
                                          info <- lift $ makeOneMailCommit CommitMailFull
                                                db ref cCommitHash (Map.lookup cCommitHash numbersMap)
                                          let mailinfoAndAction = (action, fmap fst info)
                                              action = do
                                                  markSeen syncOp cCommitHash
                                                  modifyIORef' updatedRefsI $ Map.insert ref cCommitHash
                                                  updateRefsMap
                                                  case info of
                                                      Right (MailInfo {..}, CommitInfo{..}) ->
                                                          putInexactDiffHashInDB db ciInexactDiffHash
                                                      Left _ -> return ()
                                          modifyIORef' mailsI ((:) mailinfoAndAction)
                           | otherwise -> putStrLn $ "  Skipping old commit " ++ (T.unpack shownCommitHash)

        mails <- fmap reverse $ readIORef mailsI
        lift $ sendMails mails

        finalRefs <- readIORef updatedRefsI
        when (finalRefs /= refsMap || initTracking) $ do
            writeIORef updatedRefsI refsMap
            updateRefsMap

    where
        iterateRefCommits repoPath topCommit mOldRef branchPoints = do
            (mOldRefOid, mergeBases) <- case mOldRef of
                Nothing -> return $ (Nothing, [])
                Just oldRef -> do
                    x <- fmap Just $ liftIO $ GIT.textToOid oldRef
                    y <- let normal =
                                 fmap T.lines $ lift $ gitCmd ["show-branch", "--merge-base",
                                                               oldRef, topCommit]
                             excp (_ :: ReadProcessFailed) = do
                                 -- No merge base
                                 return []
                          in  E.catch normal excp
                    mergeBases <- liftIO $ mapM GIT.textToOid y
                    return (x, mergeBases)

            refModI <- newIORef RefModNoFastForward
            newCommitsListI <- newIORef []
            alreadySeenI <- newIORef Set.empty

            let nonRootBranchPoints =
                    catMaybes $
                         map (\case (x, Just y) -> Just (x, y) ;
                                    (_, Nothing) -> Nothing) $ Set.toList $ branchPoints
                nonRootBranchPoints' = map fst $ nonRootBranchPoints
                branchPointsWithBase = Set.fromList $
                    case nonRootBranchPoints' of
                         [] -> mergeBases
                         _ ->  nonRootBranchPoints'
                isNotBranchPoint = not . ((flip Set.member) branchPointsWithBase)

                -- Iterate the history from the new ref down to all branch point,
                -- where branch point can be one of the branch points we have
                -- calculated earlier, or if there aren't any, the lowest merge
                -- base with the older version of the ref.

                u = GIT.iterateHistoryUntil True $ \notReachedOldRef commit parents -> do
                        if isNotBranchPoint commit
                            then do let commitT = GIT.oidToText commit
                                    alreadySeen <- fmap not $ markInIORefSet alreadySeenI commitT

                                    case mOldRefOid of
                                        Nothing -> return ()
                                        Just oldRefOid ->
                                            when (oldRefOid `elem` parents) $ do
                                                writeIORef refModI RefModFastForward

                                    let notReachedOldRef' =
                                            notReachedOldRef && Just commit /= mOldRefOid
                                        thisCommitsInfo = CommitIterationInfo commitT notReachedOldRef' False

                                    when (length parents <= 1) $ do
                                        modifyIORef' newCommitsListI ((:) thisCommitsInfo)

                                    nextParents <- flip filterM parents $ \parent ->
                                        case isNotBranchPoint parent of
                                            True -> return True
                                            False -> do
                                                let notReachedOldRef'' =
                                                        notReachedOldRef' && Just parent /= mOldRefOid
                                                let parentCommitsInfo =
                                                      CommitIterationInfo
                                                        (GIT.oidToText parent) notReachedOldRef'' True
                                                modifyIORef' newCommitsListI ((:) parentCommitsInfo)
                                                return False

                                    return $ (notReachedOldRef', if not alreadySeen then nextParents else [])
                            else return (notReachedOldRef, [])

            if Just topCommit /= mOldRef
               then do  _ <- u repoPath topCommit -- TODO handle error
                        r <- readIORef newCommitsListI
                        refMod <- readIORef refModI
                        return $ Just (refMod,
                                       nonRootBranchPoints,
                                       maybe NewRef ModifiedRef mOldRef,
                                       r)
               else return Nothing

checkBranchPoints :: (MonadGitomail m) => m ()
checkBranchPoints = do
    refs <- getAutoMailerRefs
    fp <- getRepositoryPath
    (branchTop, _) <-
        liftIO $ GIT.analyseRefs fp refs >>= \case
            Left l -> E.throw $ UnexpectedGitState l
            Right r -> return r

    forM_ (Map.toList branchTop) $ \(ref, branchPoints) -> do
        print (ref, branchPoints)

    return ()

showAutoMailerRefs :: (MonadGitomail m) => m ()
showAutoMailerRefs = do
    refs <- getAutoMailerRefs
    forM_ refs $ \lst -> do
        putStrLn "--"
        forM_ lst $ \(ref, hash) -> do
            putStrLn $ concat $ [T.unpack hash, " ", T.unpack ref]

