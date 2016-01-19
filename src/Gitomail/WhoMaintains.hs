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
{-# LANGUAGE TypeSynonymInstances      #-}

module Gitomail.WhoMaintains (
    iterateFilesWithMaintainers
    , whoMaintains
    , showEffectiveDefs
  )
  where

------------------------------------------------------------------------------------
import           Control.Lens.Operators     ((&), (^.))
import           Control.Monad              (forM, forM_)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.State.Strict (gets)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import           Data.List                  (intersperse)
import qualified Data.Map                   as Map
import           Data.Maybe                 (fromMaybe)
import qualified Data.Set                   as Set
import           System.FilePath            ((</>))
----
import           Gitomail.Gitomail
import qualified Gitomail.Maintainers       as Maintainers
import qualified Gitomail.Opts              as O
import qualified Lib.Git                    as GIT
import           Lib.LiftedPrelude
------------------------------------------------------------------------------------

iterateFilesWithMaintainers
    :: (MonadIO m, Monoid a) =>
       GIT.Tree t -> (BS8.ByteString -> GIT.Tree t -> m a) -> m a
iterateFilesWithMaintainers matched f = do
    let
        recurse path i@(GIT.Node _ m) = do
            l <- forM (Map.toList m) $ \(k, a) -> do
                let
                  c = case path of
                         "" -> [k]
                         _ -> [path, "/", k]
                recurse (BS.concat c) a
            b <- f path i
            case l of
                [] -> return b
                _ -> return $ (foldl1 mappend l) `mappend` b
        recurse path i = f path i
    recurse "" matched

whoMaintains :: (MonadGitomail m) => m ()
whoMaintains = do
    opts <- gets opts
    let gitRef = opts ^. O.gitRef & fromMaybe "HEAD" -- TODO: should be the index by default
    matched <- matchFiles gitRef
    let
        func path (GIT.File i) =
            do
                BS.putStr $ BS.concat [path, ": "]
                let Maintainers.AssignedFileStatus mb oberbs revs = i
                case mb of
                    Nothing -> BS.putStr $ "<no-maintainer>"
                    Just (_, s) -> BS.putStr $ s

                let printlst _ [] = return ()
                    printlst name xs = do
                        BS.putStr $ BS.concat [" ; ", name, ": "]
                        BS.putStr $ BS.concat $ intersperse ", " (map snd xs)

                printlst "Reviewers" revs
                printlst "Observers" oberbs
                BS8.putStrLn ""
        func _ _ = return ()

    liftIO $ iterateFilesWithMaintainers matched func

showEffectiveDefs :: (MonadGitomail m) => m ()
showEffectiveDefs = do
    opts <- gets opts
    repoPath <- getRepositoryPath
    let gitRef = opts ^. O.gitRef & fromMaybe "HEAD"
    patternsCompiled <- compilePatterns gitRef
    matched <- matchFiles gitRef
    avail <- Maintainers.getAvailableDefs patternsCompiled
    used <- Maintainers.getEffectiveDefs matched
    putStrLn "List of definitions affecting no files or overriden by other definitions:"
    putStrLn ""
    liftIO $ forM_ (Set.toList $ Set.difference avail used) $ \(sub, line_idx) -> do
        let rel_path = BS8.unpack sub </> BS8.unpack Maintainers.fileName
        let src_path = repoPath </> rel_path
        fileContent <- BS.readFile src_path
        let line = BS8.unpack $ (BS8.lines fileContent) !! (line_idx - 1)
        putStrLn $ rel_path ++ ":" ++ (show line_idx) ++ ":" ++ line

