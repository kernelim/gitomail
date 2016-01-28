{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE CPP                       #-}

module Gitomail.Config
       ((^.||)
       , Config
       , final
       , combine
       , parse

       -- Lens
       , sMTPHostname
       , sMTPPort
       , sMTPStartTls
       , sMTPUsername
       , sMTPPassword
       , excludeRefs
       , includeRefs
       , rootRefs
       , aliasRefMatch
       , commitSubjectLine
       , summarySubjectLine
       , commitURL
       , blobInCommitURL
       , fromEMail
       , filteredDestEMails
       , repoName
       , sourceHighlight
       , hashSize
       , hashMap
       , testRunId
       )
       where

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

defaultRootRefs :: [Text]
defaultRootRefs = ["tags/.*", "heads/master"]

defaultAliasMatch :: Maybe Text
defaultAliasMatch = (Just "heads/[^/]+/for/%a(/.+)?")

fmdString :: Text
fmdString = "filtered_email_destinations"

#define FIELDS(X)                                                                                  \
  X(_sMTPHostname       , "smtp_hostname"       , Maybe , pass,                  , Text          ) \
  X(_sMTPPort           , "smtp_port"           , a     , defl, 587              , Int           ) \
  X(_sMTPStartTls       , "smtp_starttls"       , a     , defl, True             , Bool          ) \
  X(_sMTPUsername       , "smtp_username"       , Maybe , pass,                  , Text          ) \
  X(_sMTPPassword       , "smtp_password"       , Maybe , pass,                  , Text          ) \
  X(_excludeRefs        , "exclude_refs"        , Maybe , pass,                  , [Text]        ) \
  X(_includeRefs        , "include_refs"        , Maybe , pass,                  , [Text]        ) \
  X(_rootRefs           , "root_refs"           , a     , defl, defaultRootRefs  , [Text]        ) \
  X(_aliasRefMatch      , "alias_ref_match"     , a     , defl, defaultAliasMatch, Maybe Text    ) \
  X(_commitSubjectLine  , "commit_subject_line" , a     , defl, "[%r %b %h%n] %s", Text          ) \
  X(_summarySubjectLine , "summary_subject_line", a     , defl, "[%r] %s"        , Text          ) \
  X(_commitURL          , "commit_url"          , Maybe , pass,                  , Text          ) \
  X(_blobInCommitURL    , "blob_in_commit_url"  , Maybe , pass,                  , Text          ) \
  X(_fromEMail          , "from_email"          , Maybe , pass,                  , Text          ) \
  X(_filteredDestEMails , fmdString             , a     , defl, []               , [Text]        ) \
  X(_repoName           , "repo_name"           , Maybe , pass,                  , Text          ) \
  X(_sourceHighlight    , "source_highlight"    , a     , defl, True             , Bool          ) \
  X(_hashSize           , "hash_size"           , a     , defl, 9                , Int           ) \
  X(_hashMap            , "hash_map"            , Maybe , pass,                  , Map Text Text ) \
  X(_testRunId          , "test_run_id"         , a     , defl, 0                , Int           ) \

#define X1(_name, s, f, p, v, t)  ,_name :: f (t)
data ConfigA a = Config { __unused :: () FIELDS(X1) }

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

#define X2(_name, s, f, p, v, t)   <*> x _name
combine :: ConfigA Maybe -> ConfigA Maybe -> ConfigA Maybe
combine a b = runIdentity $ Config
    <$> (pure ())
    FIELDS(X2)
   where
       rightIfJust _ b'@(Just _) = b'
       rightIfJust a' Nothing = a'
       x f = pure $ f a `rightIfJust` f b

#define X3(_name, s, f, p, v, t)    <*> p _name v
final :: ConfigA Maybe -> Config
final a = runIdentity $ Config
    <$> (pure ())
    FIELDS(X3)
    where
        defl f v = pure $ maybe (Id v) Id (f a)
        pass f = pure $ f a

#define X4(_name, s, f, p, v, t)        field s >>= \_name ->
instance FromJSON (ConfigA Maybe) where
    parseJSON v =
        do o <- parseJSON v
           let field name = o .:? name .!= Nothing
               __unused = ()
           (FIELDS(X4) return $ Config {..})

parse :: MonadIO m => String -> m (ConfigA Maybe)
parse str =
    case str of
       ('@':args) -> let (cmd:cmdargs) = T.splitOn " " $ T.pack args
                      in wrap $ fmap (Yaml.decodeEither' . T.encodeUtf8)
                         (readProcess (T.unpack cmd) cmdargs)
       fp         -> wrap $ Yaml.decodeFileEither fp
    where
        wrap dec = liftIO $ dec >>= either throwM return
