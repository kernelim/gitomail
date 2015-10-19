{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module Gitomail.Config where

------------------------------------------------------------------------------------
import           Control.Lens               (makeLenses)
import qualified Control.Lens               as Lens
import           Control.Monad.Catch        (throwM)
import           Control.Monad.Identity     (runIdentity)
import           Control.Monad.State.Strict (MonadIO, liftIO)
import           Control.Lens.Operators     ((&), (^.))
import           Data.Aeson                 (FromJSON (..), (.!=), (.:?))
import           Data.Map                   (Map)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Yaml                  as Yaml
----
import           Lib.Process                (readProcess)
------------------------------------------------------------------------------------

data ConfigA a = Config
  { _sMTPHostname       :: Maybe Text
  , _sMTPPort           :: a     Int
  , _sMTPStartTls       :: a     Bool
  , _sMTPUsername       :: Maybe Text
  , _sMTPPassword       :: Maybe Text
  , _excludeRefs        :: Maybe [Text]
  , _includeRefs        :: Maybe [Text]
  , _rootRefs           :: a     [Text]
  , _commitSubjectLine  :: a     Text
  , _summarySubjectLine :: a     Text
  , _commitURL          :: Maybe Text
  , _fromEMail          :: Maybe Text
  , _repoName           :: Maybe Text
  , _hashSize           :: a     Int
  , _hashMap            :: Maybe (Map Text Text)
  }
type Config = ConfigA Id

deriving instance Show (ConfigA Maybe)
deriving instance Show (Config)
data Id a = Id !a

fromId :: Id a -> a
fromId (Id a) = a

(^.||) :: forall b s. s -> Lens.Getting (Id b) s (Id b) -> b
a ^.|| f = a ^. f & fromId

deriving instance Show a => Show (Id a)

makeLenses ''ConfigA

combine :: ConfigA Maybe -> ConfigA Maybe -> ConfigA Maybe
combine a b = runIdentity $ Config
    <$> x _sMTPHostname
    <*> x _sMTPPort
    <*> x _sMTPStartTls
    <*> x _sMTPUsername
    <*> x _sMTPPassword
    <*> x _excludeRefs
    <*> x _includeRefs
    <*> x _rootRefs
    <*> x _commitURL
    <*> x _fromEMail
    <*> x _commitSubjectLine
    <*> x _summarySubjectLine
    <*> x _repoName
    <*> x _hashSize
    <*> x _hashMap
   where
       rightIfJust _ b'@(Just _) = b'
       rightIfJust a' Nothing = a'
       x f = pure $ f a `rightIfJust` f b

final :: ConfigA Maybe -> Config
final a = runIdentity $ Config
    <$> pass _sMTPHostname
    <*> defl _sMTPPort      587
    <*> defl _sMTPStartTls  True
    <*> pass _sMTPUsername
    <*> pass _sMTPPassword
    <*> pass _excludeRefs
    <*> pass _includeRefs
    <*> defl _rootRefs            ["tags/.*", "heads/master"]
    <*> defl _commitSubjectLine   "[%r %b %h%n] %s"
    <*> defl _summarySubjectLine  "[%r] %s"
    <*> pass _commitURL
    <*> pass _fromEMail
    <*> pass _repoName
    <*> defl _hashSize     9
    <*> pass _hashMap
    where
        defl f v = pure $ maybe (Id v) Id (f a)
        pass f = pure $ f a

instance FromJSON (ConfigA Maybe) where
    parseJSON v =
        do o <- parseJSON v
           let field name = o .:? name .!= Nothing
           Config <$> field "smtp_hostname"
                  <*> field "smtp_port"
                  <*> field "smtp_starttls"
                  <*> field "smtp_username"
                  <*> field "smtp_password"
                  <*> field "exclude_refs"
                  <*> field "include_refs"
                  <*> field "root_refs"
                  <*> field "commit_subject_line"
                  <*> field "summary_subject_line"
                  <*> field "commit_url"
                  <*> field "from_email"
                  <*> field "repo_name"
                  <*> field "hash_size"
                  <*> field "hash_map"

parse :: MonadIO m => String -> m (ConfigA Maybe)
parse str =
    case str of
       ('@':args) -> let (cmd:cmdargs) = T.splitOn " " $ T.pack args
                      in wrap $ fmap (Yaml.decodeEither' . T.encodeUtf8)
                         (readProcess (T.unpack cmd) cmdargs)
       fp         -> wrap $ Yaml.decodeFileEither fp
    where
        wrap dec = liftIO $ dec >>= either throwM return