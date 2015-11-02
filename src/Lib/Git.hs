{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE GADTs #-}

module Lib.Git
  ( lsFiles
  , Tree(..)
  , TreeMap
  , mapTreeM
  , mapTreeMaybeM
  , oidToText
  , textToOid
  , foldTree
  , printTree
  , readBlob
  , treeVal
  , treeMap
  , refRepr
  , analyseRefs
  , Git.RefName
  , GitCommitHash
  , GitOid
  , parseOid
  , iterateHistoryUntil
  ) where

------------------------------------------------------------------------------------
import           Control.Monad             (forM_, when)
import           Control.Lens.Operators    ((&))
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted    as E
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import Data.Maybe (fromMaybe)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import           Git                       (catBlob, commitTree,
                                            listTreeEntries, lookupObject,
                                            lookupReference, lookupTree,
                                            referenceToOid, withRepository,
                                            parseOid)
import qualified Git                       as Git
import           Git.Libgit2               (lgFactory, OidPtr,
                                           shaToOid)
import           Control.Monad.State       (MonadIO)
import           Control.Monad             (foldM)
import Data.Tagged (Tagged(..))
import qualified Data.Sequence             as Seq
import           Data.Text           (Text)
----
import           Lib.Monad                 (mapWithKeyM)
import           Lib.LiftedPrelude
------------------------------------------------------------------------------------

type GitCommitHash = Text
type GitOid = Git.Libgit2.OidPtr

type TreeMap a = Map BS.ByteString (Tree a)

data Tree a
   = Node a (TreeMap a)
   | File a
     deriving (Show, Functor)

treeVal :: Tree t -> t
treeVal (File i) = i
treeVal (Node i _) = i

treeMap :: Tree t -> TreeMap t
treeMap (File _) = Map.empty
treeMap (Node _ x) = x

loadAnyRevStr :: (MonadIO m, Git.MonadGit r m, MonadMask m, MonadBaseControl IO m) =>
                  T.Text -> EitherT String m (Git.Object r m)
loadAnyRevStr revstr =
    E.catch (lift $ parseOid revstr >>= lookupObject) $ \(_ :: Git.GitException) -> do
        ref <- try_io_action (lookupReference revstr) $ "Could not resolve " ++ T.unpack revstr
        oid <- try_io_action (referenceToOid ref) $ "Could not resolve Oid of " ++ T.unpack revstr
        lift $ lookupObject oid
    where
        try_io_action act err = lift act >>= maybe (left err) right

oidToText :: GitOid -> T.Text
oidToText = T.pack . show

textToOid :: Text -> IO OidPtr
textToOid t = Git.textToSha t >>= shaToOid

iterateHistoryUntil :: forall (m :: * -> *) (t :: * -> *) a.
                       (Foldable t, MonadIO m, MonadBaseControl IO m, MonadMask m)
                       => a
                       -> (a -> GitOid -> [GitOid] -> IO (a, t (GitOid)))
                       -> FilePath
                       -> GitCommitHash
                       -> m (Either String ())
iterateHistoryUntil v'' f path firstrev = do
    withRepository lgFactory path $ do
        let filterF v  oid parents act = do
                b <- lift $ liftIO $ f v oid (map (\(Tagged parent) -> parent) parents)
                act b
        let recurse v (Git.CommitObj commit) = do
                let Tagged oid = Git.commitOid commit
                filterF v oid (Git.commitParents commit) $ \(v', parents) ->
                    forM_ parents $ \parent -> do
                        sub_object <- lift $ lookupObject parent
                        recurse v' sub_object
            recurse _ _ = left $ "Not a commit object while iterating hashes"
        runEitherT $ do
            oid <- lift $ Git.parseOid firstrev
            (lift $ lookupObject oid) >>= (recurse v'')

readBlob :: (MonadIO m, MonadBaseControl IO m, MonadMask m) =>
            FilePath -> Text -> m BS8.ByteString
readBlob path blobIndex = do
    withRepository lgFactory path $ do
        Git.parseObjOid blobIndex >>= catBlob

type RefAnalysis a = (Map a (Set (OidPtr, Maybe a)),
                      Map a (Set (OidPtr, a)))

analyseRefs :: forall (m :: * -> *) (t :: * -> *) a .
     (Ord a, Foldable t, MonadIO m,
      MonadBaseControl IO m, MonadMask m) =>
     FilePath
     -> t (t (a, GitCommitHash))
     -> m (Either String (RefAnalysis a))
analyseRefs path startRefsList = do
    withRepository lgFactory path $ do
        let getParentsOids oid =
                (lift $ lookupObject oid) >>= \case
                    (Git.CommitObj commit) -> do
                        let parents = Git.commitParents commit
                        return $ map (\(Tagged parent) -> parent) parents
                    _ -> left $ "Not a commit object while iterating hashes"

        let upsertIORefMap mRef k def f = do
                m <- readIORef mRef
                let v = fromMaybe def (Map.lookup k m)
                writeIORef mRef (Map.insert k (f v) m)

        let insertIORefMap mRef k v = do
                m <- readIORef mRef
                writeIORef mRef (Map.insert k v m)

        let lookupCommit mRef k = do
                m <- readIORef mRef
                return $ Map.lookup k m

        branchPointsTopI <- newIORef Map.empty
        branchPointsBottomI <- newIORef Map.empty
        allCommitsI <- newIORef Map.empty

        let loopUpper =
                forM_ startRefsList $ \startRefs -> do
                    newCommitsI <- do
                        initCommitsI <- newIORef Map.empty
                        forM_ startRefs $ \(refname, hash) -> do
                            oid' <- lift $ Git.parseOid hash
                            (lift $ lookupObject oid') >>= \case
                                (Git.CommitObj commit) -> do
                                    let Tagged oid = Git.commitOid commit
                                    upsertIORefMap initCommitsI oid Set.empty $ Set.insert refname
                                _ -> return ()
                        commits <- readIORef initCommitsI
                        let f (oid, refset) = map (\ref->(oid, ref)) $ Set.toList refset
                        newIORef $ Seq.fromList $ concat $ map f $ Map.toList commits

                    let loop = do
                            refs <- readIORef newCommitsI
                            when (refs & Seq.null & not) $ do
                                -- TODO: We can consult a DB to see if refs are only
                                -- reachable from roots, i.e. there will be no further
                                -- updaets of branchPoints*.
                                writeIORef newCommitsI Seq.empty
                                forM_ refs $ \(oid, ref) -> do
                                    let update parent branchingRef = do
                                            when (ref /= branchingRef) $ do
                                                upsertIORefMap branchPointsTopI ref Set.empty $
                                                     Set.insert $ (parent, Just branchingRef)
                                                upsertIORefMap branchPointsBottomI branchingRef Set.empty $
                                                     Set.insert $ (oid, ref)
                                    lookupCommit allCommitsI oid >>= \case
                                        Nothing -> do
                                            insertIORefMap allCommitsI oid ref
                                            parents <- getParentsOids oid
                                            case parents of
                                                [] -> upsertIORefMap branchPointsTopI ref Set.empty $
                                                          Set.insert $ (oid, Nothing)
                                                _ ->  forM_ parents $ \parent ->
                                                          lookupCommit allCommitsI parent >>= \case
                                                              Nothing -> modifyIORef'
                                                                         newCommitsI (Seq.|> (parent, ref))
                                                              Just branchingRef ->
                                                                   update parent branchingRef
                                        Just branchingRef -> do
                                            update oid branchingRef
                                loop
                    loop

        runEitherT loopUpper >>= \case
            Left s -> return $ Left s
            Right () -> do
                branchPointsTop <- readIORef branchPointsTopI
                branchPointsBottom <- readIORef branchPointsBottomI
                return $ Right (branchPointsTop, branchPointsBottom)


lsFiles
  :: (MonadIO m, MonadMask m, MonadBaseControl IO m)
     => FilePath
     -> Git.RefName
     -> (Git.TreeFilePath -> Bool)
     -> Maybe BS8.ByteString
     -> m (Either String (Tree (Maybe BS8.ByteString)))
lsFiles path revstr blob_filter dir_value = do
    withRepository lgFactory path $ do
        let
            blob_reader fp blob_id = do
                if blob_filter fp
                    then fmap Just $ catBlob blob_id
                    else return Nothing
            func treeoid = do
                tree <- lookupTree treeoid
                entries <- listTreeEntries tree
                r <- newIORef []
                let append x = modifyIORef' r ((:) x)
                forM_ entries $ \(fp, entry) -> do
                    when (not ('/' `BS8.elem` fp)) $ do
                        case entry of
                          Git.TreeEntry sub_treeoid -> do
                              lst <- func sub_treeoid
                              append $ (fp, Node dir_value lst)
                          Git.BlobEntry blob_id _ -> do
                              x <- blob_reader fp blob_id
                              append $ (fp, File x)
                          _ -> return ()
                fmap (Map.fromList) $ liftIO $ readIORef r

        runEitherT $ do
            i <- loadAnyRevStr revstr
            case i of
                Git.CommitObj commit ->
                    fmap (Node dir_value) $ lift $ func (commitTree commit)
                _ -> left $ "Could not resolve tree"


mapTreeM :: Monad m => ([BS.ByteString] -> a -> m b) -> Tree a -> m (Tree b)
mapTreeM f tree = r [] tree
    where r p (Node i m) = Node <$> (f p i) <*> (mapWithKeyM (\name a -> r (name:p) a) m)
          r p (File i) = File <$> (f p i)

mapTreeMaybeM :: Monad m => ([BS.ByteString] -> a -> m b) -> Tree (Maybe a) -> m (Tree (Maybe b))
mapTreeMaybeM f tree = mapTreeM s tree
    where s _ Nothing = return Nothing
          s p (Just i) = fmap Just (f p i)

foldTree :: Monad m => b -> ([BS.ByteString] -> b -> a -> m b) -> Tree a -> m b
foldTree s f tree = r [] s tree
    where r p s' (Node i m) =
              do s'' <- f p s' i
                 foldM (\t (name, a) -> r (name:p) t a) s'' (Map.toList m)
          r p s' (File i) = f p s' i

printTree :: Show a => Tree a -> IO ()
printTree tree =
    r (0 :: Int) tree
  where
    r d t = do
      case t of
          Node i m -> do
              putStrLn (show i)
              forM_ (Map.toList m) $ \(k, a) -> do
                  indent
                  BS.putStr k
                  BS.putStr " : "
                  r (d + 1) a
          File i -> do
              putStrLn (show i)
      where
          indent = forM_ [0..d] $ \_ -> (BS.putStr "  ")

refRepr :: Text -> Text
refRepr ref  = do
    case T.stripPrefix "heads/" ref of
        Just branch -> T.concat [branch, " branch"]
        Nothing -> case T.stripPrefix "tags/" ref of
            Just tag -> T.concat [tag, " tag"]
            Nothing -> T.concat [ref, " ref"]
