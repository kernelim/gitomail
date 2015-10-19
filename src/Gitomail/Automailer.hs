{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf                #-}

module Gitomail.Automailer
    ( autoMailer
    , getAutoMailerRefs
    , showAutoMailerRefs
    , checkBranchPoints
    , UnexpectedGitState(..)
    ) where

------------------------------------------------------------------------------------
import qualified Control.Exception.Lifted    as E
import           Control.Lens.Operators      ((^.), (&))
import           Control.Monad               (forM, forM_, when, filterM)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Char8       as BS8
import           Data.List                   (sortOn, groupBy, nub, intersperse)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe, catMaybes)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Sequence               as Seq
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
import qualified Lib.Git                     as GIT
import qualified Lib.InlineFormatting        as F
import           Lib.Regex                   (matchWhole)
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
      , cIsNew               :: !Bool
      , cNotReachedOldRef    :: !Bool
      , cIsBranchPoint       :: !Bool
      } deriving Show

makeHeaderMail :: (MonadGitomail m)
                  => DB
                  -> (Text, GIT.GitCommitHash)
                  -> RefModState
                  -> RefState
                  -> [CommitIterationInfo]
                  -> [(GIT.GitOid, O.GitRef)]
                  -> m ((Map.Map GIT.GitCommitHash Int, Either String MailInfo))
makeHeaderMail db (ref, topCommit) refMod isNewRef commits nonRootBranchPoints = do
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
                   -- print (cCommitHash, cIsBranchPoint, cNotReachedOldRef, cIsNew)
                   let getNr = do
                           n <- readIORef numberingI
                           writeIORef numberingI (n + 1)
                           return n
                   case (cIsBranchPoint, cNotReachedOldRef, cIsNew) of
                       (True, _, _)  -> modifyIORef' branchPointsI ((Nothing, cCommitHash) :)
                       (_, False, _) -> do nr <- getNr
                                           modifyIORef' belowOrEqOldRefI ((Just nr, cCommitHash) :)
                       (_, _, True)  -> do nr <- getNr
                                           modifyIORef' newCommitsI ((Just nr, cCommitHash) :)
                       _ -> return ()

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
                        ("Content"          , newCommits)
                      , ("Previously pushed", belowOrEqOldRef)
                      , ("Branch points"    , branchPoints)
                      ]
                let insert sI x = modifyIORef' sI (Seq.|> x)

                flistI <- newIORef Seq.empty
                mailsI <- newIORef Seq.empty
                -- insert flistI $ ("AD", [])

                forM_ commitLists $ \(name, list) -> do
                   emptySoFarI <- newIORef True
                   forM_ list $ \(maybeNr, commitHash) -> do
                       mailinfo <- makeOneMailCommit CommitMailSummary db commitHash maybeNr
                       case mailinfo of
                           Right (MailInfo{..}) -> do
                               whenM (readIORef emptySoFarI) $ do
                                   writeIORef emptySoFarI False
                                   insert flistI (T.concat ["\n", name, "\n"], [])

                               mappedCommitHash <- mapCommitHash commitHash
                               githash <- githashRepr mappedCommitHash
                               insert mailsI miMail
                               case maybeNr of
                                   Just nr -> do
                                       modifyIORef' githashtoNumberI (Map.insert commitHash nr)
                                       insert flistI (T.pack $ "#" ++ show nr ++ " ",
                                                      [F.List, F.ListItem, F.Monospace])
                                   Nothing -> return ()

                               links <- getCommitURL mappedCommitHash >>= \case
                                   Nothing -> return []
                                   Just url -> return [F.Link url]
                               insert flistI (githash, [F.List, F.ListItem, F.Monospace
                                                       ] ++ links)
                               insert flistI (": ", [F.List, F.ListItem])
                               insert flistI (fromMaybe "" miCommmitSubject, [F.List, F.ListItem])
                               insert flistI ("\n", [F.List])
                           Left _ ->
                               return ()

                flist <- readIORef flistI
                mails <- readIORef mailsI
                emailFooter <- getFooter
                return (toList flist ++ emailFooter, toList $ mails)

            (cc, to) <- getExtraCCTo

            emailAddress <- getFromEMail
            let toAddreses = nub $ to ++ (concat $ map mailTo mails)
                ccAddresses = nub $ cc ++ (concat $ map mailCc mails)
                (toAddreses', ccAddreses') =
                    if toAddreses == [] then (ccAddresses, [])
                                        else (toAddreses, ccAddresses)

            repoName <- getRepoName
            githashToNumber <- readIORef githashtoNumberI
            case toAddreses of
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
                          , mailTo      = toAddreses'
                          , mailCc      = ccAddreses'
                          , mailBcc     = []
                          , mailHeaders = [("Subject", subjectLine)]
                          , mailParts   = [[plainPart plain, htmlPart html]]
                          }
                    return (githashToNumber, Right $ MailInfo mail "" subjectLine Nothing)
        False ->
            return $ (Map.empty, Left "No commits, not sending anything")

getAutoMailerRefs :: (MonadGitomail m) => m [[(O.GitRef, GIT.GitCommitHash)]]
getAutoMailerRefs = do
    refsMatcher <- getRefsMatcher
    refScore <- getRefScoreFunc
    let onlyRefs x = refsMatcher x

    refsLines <- fmap T.lines $ gitCmd ["show-ref", "--heads", "--tags", "--dereference"]
    byScores <- fmap catMaybes $ forM refsLines $ \line -> do
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

refsMapDBKey :: BS8.ByteString
refsMapDBKey = "refs-map"

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

        -- FIXME: sensitive to how hackage git does 'read' / 'show'. We should newtype.
        mOldRefs <- DB.get db DB.defaultReadOptions refsMapDBKey
        let initTracking = (mOldRefs == Nothing)
        when initTracking $ do
            putStrLn "Initial save of ref map. Will start tracking refs from now (this may take awhile)"

        let oldRefsMap = maybe refsMap (read . BS8.unpack) mOldRefs
        alreadySeenI <- newIORef Set.empty

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
                    fmap (fmap (\x -> ((ref, topCommit), x))) $ do
                        iterateRefCommits repoPath db topCommit
                            (if initTracking then Nothing else Map.lookup ref oldRefsMap)
                            branchPoints alreadySeenI

        mailsI <- newIORef []

        updatedRefsI <- newIORef oldRefsMap
        let updateRefsMap = do
                x <- readIORef updatedRefsI
                putDB (DB.defaultWriteOptions {DB.sync = True })
                    refsMapDBKey (BS8.pack $ show x)

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

                    (numbersMap, summaryMailInfo) <-
                        lift $ makeHeaderMail db (ref, topCommit) refMod
                            isNewRef commitsinfo nonRootBranchPoints
                    modifyIORef' mailsI ((:) (return (), summaryMailInfo))

                    forM_ commitsinfo $ \CommitIterationInfo {..} -> do
                        shownCommitHash <- lift $ mapCommitHash cCommitHash
                        if | cIsNew -> do putStrLn $ "  Formatting " ++ (T.unpack shownCommitHash)
                                          mailinfo <- lift $ makeOneMailCommit CommitMailFull
                                                db cCommitHash (Map.lookup cCommitHash numbersMap)
                                          let mailinfoAndAction = (action, mailinfo)
                                              action = do
                                                  markSeen syncOp cCommitHash
                                                  modifyIORef' updatedRefsI $ Map.insert ref cCommitHash
                                                  updateRefsMap
                                                  case mailinfo of
                                                      Right (MailInfo {..}) ->
                                                          putInexactDiffHashInDB db miInexactDiffHash
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
        iterateRefCommits repoPath db topCommit mOldRef branchPoints alreadySeenI = do
            (mOldRefOid, mergeBases) <- case mOldRef of
                Nothing -> return $ (Nothing, [])
                Just oldRef -> do
                    x <- fmap Just $ liftIO $ GIT.textToOid oldRef
                    y <- fmap T.lines $ lift $ gitCmd ["show-branch", "--merge-base",
                                                       oldRef, topCommit]
                    mergeBases <- liftIO $ mapM GIT.textToOid y
                    return (x, mergeBases)

            refModI <- newIORef RefModNoFastForward
            newCommitsListI <- newIORef []
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
                                    (isNew, alreadySeen) <- do
                                        v <- DB.get db DB.defaultReadOptions
                                            $ commitSeenKey $ T.encodeUtf8 commitT
                                        alreadySeenSet <- readIORef alreadySeenI
                                        let isNew = not alreadySeen && maybe True (const False) v
                                            alreadySeen = commit `Set.member` alreadySeenSet
                                        when (not alreadySeen) $
                                            writeIORef alreadySeenI (commit `Set.insert` alreadySeenSet)
                                        return (isNew, alreadySeen)

                                    case mOldRefOid of
                                        Nothing -> return ()
                                        Just oldRefOid ->
                                            when (oldRefOid `elem` parents) $ do
                                                writeIORef refModI RefModFastForward

                                    let notReachedOldRef' =
                                            notReachedOldRef && Just commit /= mOldRefOid
                                        thisCommitsInfo = CommitIterationInfo commitT isNew notReachedOldRef' False

                                    when (length parents == 1) $ do
                                        modifyIORef' newCommitsListI ((:) thisCommitsInfo)

                                    nextParents <- flip filterM parents $ \parent ->
                                        case isNotBranchPoint parent of
                                            True -> return True
                                            False -> do
                                                let notReachedOldRef'' =
                                                        notReachedOldRef' && Just parent /= mOldRefOid
                                                let parentCommitsInfo =
                                                      CommitIterationInfo
                                                        (GIT.oidToText parent) False notReachedOldRef'' True
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
