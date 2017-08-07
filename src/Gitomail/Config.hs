{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}

module Gitomail.Config
       ((^.||)
       , Config
       , JIRACC(..)
       , final
       , combine
       , parse

       -- Lens
       , aliasRefMatch
       , blobInCommitURL
       , commitSubjectLine
       , commitURL
       , excludeRefs
       , filteredDestEmails
       , fromEmail
       , hashMap
       , hashSize
       , includeRefs
       , issueTrackMatch
       , issueTrackURL
       , jiraCC
       , maxEmailSize
       , repoName
       , rootRefs
       , sMTPHostname
       , sMTPPassword
       , sMTPPort
       , sMTPStartTls
       , sMTPUsername
       , sourceHighlight
       , summarySubjectLine
       , testRunId
       )
       where

------------------------------------------------------------------------------------
import           Control.Lens               (makeLenses)
import qualified Control.Lens               as Lens
import           Control.Lens.Operators     ((&), (^.))
import           Control.Monad.Catch        (throwM)
import           Control.Monad.Identity     (runIdentity)
import           Control.Monad.State.Strict (MonadIO, liftIO)
import           Data.Aeson                 (FromJSON (parseJSON), (.:?),
                                             ToJSON (toJSON), Value (Object),
                                             object, (.:), (.=), (.!=))
import           Data.Map                   (Map)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Data.Yaml                  as Yaml
----

import           Lib.Process                (readProcess)
------------------------------------------------------------------------------------

defaultRootRefs :: [Text]
defaultRootRefs = ["heads/master", "tags/.*"]

defaultAliasMatch :: Maybe Text
defaultAliasMatch = (Just "heads/[^/]+/for/%a(/.+)?")

fmdString :: Text
fmdString = "filtered-email-destinations"

#define FIELDS(X)                                                                                  \
  X(_sMTPHostname       , "smtp-hostname"       , Maybe , pass,                  , Text          ) \
  X(_sMTPPort           , "smtp-port"           , a     , defl, 587              , Int           ) \
  X(_sMTPStartTls       , "smtp-starttls"       , a     , defl, True             , Bool          ) \
  X(_sMTPUsername       , "smtp-username"       , Maybe , pass,                  , Text          ) \
  X(_sMTPPassword       , "smtp-password"       , Maybe , pass,                  , Text          ) \
  X(_excludeRefs        , "exclude-refs"        , Maybe , pass,                  , [Text]        ) \
  X(_includeRefs        , "include-refs"        , Maybe , pass,                  , [Text]        ) \
  X(_rootRefs           , "root-refs"           , a     , defl, defaultRootRefs  , [Text]        ) \
  X(_aliasRefMatch      , "alias-ref-match"     , a     , defl, defaultAliasMatch, Maybe Text    ) \
  X(_issueTrackMatch    , "issue-track-match"   , Maybe , pass,                  , Text          ) \
  X(_issueTrackURL      , "issue-track-url"     , Maybe , pass,                  , Text          ) \
  X(_commitSubjectLine  , "commit-subject-line" , a     , defl, "[%r %b %h%n] %s", Text          ) \
  X(_summarySubjectLine , "summary-subject-line", a     , defl, "[%r] %s"        , Text          ) \
  X(_commitURL          , "commit-url"          , Maybe , pass,                  , Text          ) \
  X(_blobInCommitURL    , "blob-in-commit-url"  , Maybe , pass,                  , Text          ) \
  X(_fromEmail          , "from-email"          , Maybe , pass,                  , Text          ) \
  X(_jiraCC             , "jira-cc"             , Maybe , pass,                  , JIRACC        ) \
  X(_filteredDestEmails , fmdString             , a     , defl, []               , [Text]        ) \
  X(_repoName           , "repo-name"           , Maybe , pass,                  , Text          ) \
  X(_sourceHighlight    , "source-highlight"    , a     , defl, True             , Bool          ) \
  X(_hashSize           , "hash-size"           , a     , defl, 9                , Int           ) \
  X(_maxEmailSize       , "max-email-size"      , a     , defl, 8000000          , Int           ) \
  X(_hashMap            , "hash-map"            , Maybe , pass,                  , Map Text Text ) \
  X(_testRunId          , "test-run-id"         , a     , defl, 0                , Int           ) \

#define X1(_name, s, f, p, v, t)  ,_name :: f (t)
data ConfigA a = Config { __unused :: () FIELDS(X1) }

data JIRACC = JIRACC
    { jiraFields :: [Text]
    , jiraURL    :: Text
    , jiraCreds  :: Text
    } deriving Show

instance ToJSON JIRACC where
    toJSON (JIRACC fields url creds) =
        object
          [ "fields"      .== fields
          , "url"         .== url
          , "http_user"   .== creds
          ]
     where (.==) = (Data.Aeson..=)

instance FromJSON JIRACC where
    parseJSON (Object o) =
        JIRACC <$> o .: "fields"
               <*> o .: "url"
               <*> o .: "http_creds"
    parseJSON v = error $ "JIRACC parse: " ++ (show v)


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
