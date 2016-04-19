{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE GADTs                     #-}

module Lib.Git
  ( lsFiles
  , Tree(..)
  , TreeMap
  , LsFilesFilter(..)
  , mapTreeM
  , mapTreeMaybeM
  , foldTree
  , printTree
  , readBlob
  , treeVal
  , treeMap
  , refRepr
  , Git.RefName
  , CommitHash
  , iterateHistoryUntil
  ) where

------------------------------------------------------------------------------------
import           Control.Monad             (forM_, forM)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.Exception.Lifted    as E
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
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
import           Data.Tagged               (Tagged(..))
import           Data.Text                 (Text)
----
import           Lib.Monad                 (mapWithKeyM)
import           Lib.LiftedPrelude
------------------------------------------------------------------------------------

type CommitHash = Text
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
                       (Traversable t, Foldable t, MonadIO m, MonadBaseControl IO m, MonadMask m)
                       => a
                       -> (a -> CommitHash -> [CommitHash] -> IO (a, t (CommitHash)))
                       -> FilePath
                       -> CommitHash
                       -> m (Either String ())
iterateHistoryUntil v'' f path firstrev = do
    withRepository lgFactory path $ do
        let filterF v  oidH parentsH act = do
                let oid = oidToText oidH
                (a, tl) <- lift $ liftIO $ f v oid (map (\(Tagged parent) -> oidToText parent) parentsH)
                tl' <- forM tl (liftIO . textToOid)
                act (a, tl')
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

data LsFilesFilter
    = LffReadBlob
    | LffListBlob
    | LffIgnore

lsFiles
  :: (MonadIO m, MonadMask m, MonadBaseControl IO m)
     => FilePath
     -> Git.RefName
     -> (Git.TreeFilePath -> LsFilesFilter)
     -> Maybe BS8.ByteString
     -> m (Either String (Tree (Maybe BS8.ByteString)))
lsFiles path revstr blobFilter dir_value = do
    withRepository lgFactory path $ do
        let
            root treeoid = do
                entriesI <- lookupTree treeoid >>= listTreeEntries >>= newIORef

                let
                    append r x = modifyIORef' r ((:) x)
                    loop subpath ref = do
                        entries <- readIORef entriesI
                        case entries of
                            [] -> return ()
                            ((fp, entry):tail') -> do
                                let (_, bfp) = BS8.breakEnd (=='/') fp
                                case subpath `BS8.isPrefixOf` fp of
                                    False -> return ()
                                    True -> do
                                        writeIORef entriesI tail'
                                        case entry of
                                            Git.BlobEntry blob_id _ -> do
                                                case blobFilter fp of
                                                    LffReadBlob -> do
                                                        z <- catBlob blob_id
                                                        append ref $ (bfp, File (Just z))
                                                    LffListBlob -> do
                                                        append ref $ (bfp, File Nothing)
                                                    LffIgnore -> return ()
                                            Git.TreeEntry _ -> do
                                                subref <- newIORef []
                                                loop (BS8.concat [fp, "/"]) subref
                                                n <- fmap (Map.fromList) $ liftIO $ readIORef subref
                                                append ref $ (bfp, Node dir_value n)
                                            _ -> return ()
                                        loop subpath ref
                r <- newIORef []
                loop "" r
                fmap (Map.fromList) $ liftIO $ readIORef r

        runEitherT $ do
            i <- loadAnyRevStr revstr
            case i of
                Git.CommitObj commit ->
                    fmap (Node dir_value) $ lift $ root $ commitTree commit
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
