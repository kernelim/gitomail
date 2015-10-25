{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

module Gitomail.Maintainers
  ( AssignedFileStatus(AssignedFileStatus)
  , Error(..)
  , Definition(..)
  , Unit(..)
  , DefInFile
  , assignDefinitionFiles
  , compilePatterns
  , fileName
  , getAvailableDefs
  , getEffectiveDefs
  , loadFiles
  , matchFiles
  , parse
  , parseFiles
  , fsMaintainer
  , fsObservers
  , fsReviewers
  )
  where

------------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy      as BL
import qualified Control.Exception.Lifted  as E
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as BS8
import qualified Data.Map                  as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Typeable             (Typeable)
import           System.FilePath           (joinPath)
import qualified System.FilePath.Glob      as Glob
import           Control.Monad.Catch        (MonadMask)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Control.Monad.State       (MonadIO)
----
import           Gitomail.Maintainers.Parser (happyParser)
import           Gitomail.Maintainers.Base   (AliasName, EMail, Unit (..), runAlex)
import qualified Gitomail.Maintainers.Base   as MB
import qualified Lib.Git                   as GIT
import           Lib.Maybe                 (maybeF)
import           Lib.Monad                 (foldSubJoinT21toT12M)
------------------------------------------------------------------------------------

data Error = Error Int Int String
   deriving Show

type Path = BS.ByteString

parse :: BL.ByteString -> Either Error Unit
parse s =
  case runAlex s $ happyParser of
    Right x -> Right x
    Left ('l':'e':'x':xs) ->
      Left (Error 0 0 xs) -- TODO
    Left ('s':'h':'o':'w':'-':'e':'r':'r':'o':'r':':':' ':xs) ->
      let (line, column, e) = (read xs :: (Int, Int, String))
       in Left (Error line column e)
    Left xs -> Left (Error 0 0 xs)

data FailedMaintainersParse = FailedMaintainersParse String deriving (Typeable)
instance E.Exception FailedMaintainersParse
instance Show FailedMaintainersParse where
    show (FailedMaintainersParse msgstr) = "FailedMaintainersParse: " ++ msgstr

parseFiles :: Monad m => GIT.Tree (Maybe Path) -> m (GIT.Tree (Maybe Unit))
parseFiles tree = GIT.mapTreeMaybeM f tree
    where f _pathcomps content = do
            case parse (BL.fromChunks [content]) of
                Left _err -> E.throw $ FailedMaintainersParse "TODO"
                Right unit -> return unit

data Assign = Observer | Maintainer | Reviewer
  deriving (Eq, Show)

data Definition
  = Alias AliasName EMail
  | Assign Assign AliasName Glob.Pattern
  deriving (Eq, Show)

type DefInFile = (Int, Definition)

compilePatterns :: Monad m => GIT.Tree (Maybe Unit) -> m (GIT.Tree (Maybe [DefInFile]))
compilePatterns tree = GIT.mapTreeMaybeM f tree
    where
        f path (Unit defs) = return $ reverse $ map (\(ln, x) -> (ln, d x)) defs
            where
                d (MB.Alias name email) = Alias name email
                d (MB.Observer name ms) = Assign Observer name $ e ms
                d (MB.Maintainer name ms) = Assign Maintainer name  $ e ms
                d (MB.Reviewer name ms) = Assign Reviewer name  $ e ms
                e Nothing = Glob.compile (z "**/*")
                e (Just x) = Glob.compile (z (BS8.unpack x))
                z wc =
                  case path of
                    [] -> wc
                    xs -> joinPath $ reverse $ wc:(map BS8.unpack (drop 1 xs))

fileName :: Path
fileName = "Maintainers"

assignDefinitionFiles :: GIT.Tree (Maybe a) -> GIT.Tree [(BS8.ByteString, a)]
assignDefinitionFiles tree = r [] [] tree
  where r p p2 (GIT.Node _ m) = sub
          where
            sub = GIT.Node p2' $ Map.mapWithKey (\name a -> r (name:p) p2' a) m
            path = BS8.pack $ joinPath $ reverse $ map BS8.unpack p
            p2' = case Map.lookup fileName m of
                    Just (GIT.File (Just x)) -> (path, x):p2
                    _ -> p2
        r _ p2 (GIT.File _) = GIT.File p2

type DefEMail = ((Path, Int), EMail)
data AssignedFileStatus = AssignedFileStatus
    { fsMaintainer :: !(Maybe DefEMail)
    , fsObservers  :: ![DefEMail]
    , fsReviewers  :: ![DefEMail]
    } deriving Show

instance Monoid AssignedFileStatus where
    mempty = AssignedFileStatus Nothing [] []
    mappend a b =
          AssignedFileStatus {
            fsObservers = Set.toList $ Set.union (Set.fromList $ fsObservers b) (Set.fromList $ fsObservers a)
          , fsReviewers = Set.toList $ Set.union (Set.fromList $ fsReviewers b) (Set.fromList $ fsReviewers a)
          , fsMaintainer = case fsMaintainer a of
                              Nothing -> fsMaintainer b
                              _ -> fsMaintainer a
          }

fileStatusToDefSet :: AssignedFileStatus -> Set (Path, Int)
fileStatusToDefSet (AssignedFileStatus a b c) =
  Set.fromList $ map fst $ (case a of Nothing -> [] ; Just i -> [i]) ++ b ++ c

data InvalidAlias = InvalidAlias String deriving (Typeable)
instance E.Exception InvalidAlias
instance Show InvalidAlias where
  show (InvalidAlias msgstr) = "InvalidAlias: " ++ msgstr

getAvailableDefs :: Monad m => GIT.Tree (Maybe [DefInFile]) -> m (Set (Path, Int))
getAvailableDefs tree =
  do x <- root
     foldSubJoinT21toT12M x Set.empty $ add
  where root = GIT.foldTree [] f tree
        add s' (pos, Assign _ _ _) = return $ Set.insert pos s'
        add s' (_, Alias _ _) = return s'
        f _ lst Nothing = return $ lst
        f p lst (Just xs) = return $ (path, xs):lst
          where path = BS8.pack $ joinPath $ reverse $ map BS8.unpack (drop 1 p)

getEffectiveDefs :: Monad m => GIT.Tree AssignedFileStatus -> m (Set (Path, Int))
getEffectiveDefs tree = GIT.foldTree Set.empty f tree
  where f _ s content = return $ Set.union (fileStatusToDefSet content) s

matchFiles :: Monad m => GIT.Tree [(BS8.ByteString, [DefInFile])] -> m (GIT.Tree AssignedFileStatus)
matchFiles tree = GIT.mapTreeM f tree
  where f pathcomps content = root
          where
            match_options = Glob.matchDefault { Glob.matchDotsImplicitly = True }
            matcher = Glob.matchWith match_options
            empty_fs = AssignedFileStatus Nothing [] []
            path = joinPath $ reverse $ map BS8.unpack pathcomps
            root = do
              aliases <- join Map.empty $ \m (_, def) ->
                case def of
                  Alias name email -> return $ Map.insert name email m -- TODO: handle dups
                  _ -> return m
              join empty_fs $ \fs (location, def) ->
                case def of
                  Assign assignt alias pattern -> do
                    if matcher pattern path
                        then maybeF
                             (Map.lookup alias aliases)
                             (E.throw $ InvalidAlias $ BS8.unpack alias)
                                $ \email -> return $ mappend fs (case assignt of
                                              Maintainer -> empty_fs { fsMaintainer = Just (location, email) }
                                              Observer -> empty_fs { fsObservers = [(location, email)] }
                                              Reviewer -> empty_fs { fsReviewers = [(location, email)] })
                        else return fs
                  _ -> return fs
            join = foldSubJoinT21toT12M content

data FailedReadingRepo = FailedReadingRepo String deriving (Typeable)
instance E.Exception FailedReadingRepo
instance Show FailedReadingRepo where
    show (FailedReadingRepo msgstr) = "FailedReadingRepo: " ++ msgstr

loadFiles :: (MonadIO m, MonadMask m, MonadBaseControl IO m)
             => FilePath -> GIT.RefName -> m (GIT.Tree (Maybe BS8.ByteString))
loadFiles path revstr = do
    r <- GIT.lsFiles path revstr (== fileName) Nothing
    case r of
        Left err -> E.throw $ FailedReadingRepo err
        Right rx -> return rx
