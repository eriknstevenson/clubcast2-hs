{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module Clubcast.Types where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Default
import Data.Monoid
import Data.Text.Lazy (Text)
import Data.Time.Clock
import GHC.Generics
import Network.HTTP.Client.Conduit
import Network.HTTP.Types.Status

data Config = Config { manager :: Manager
                     , maxSimultaneousDownloads :: Int
                     }

newtype ClubcastT m a = ClubcastT { unClubcastT :: ReaderT Config m a }
  deriving (Functor,Applicative,Monad,MonadReader Config,MonadIO,MonadThrow,MonadCatch,MonadTrans)

deriving instance (MonadBase b m) => MonadBase b (ClubcastT m)

instance MonadBaseControl b m => MonadBaseControl b (ClubcastT m) where
  type StM (ClubcastT m) a = ComposeSt ClubcastT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

instance MonadTransControl ClubcastT where
  type StT ClubcastT a = StT (ReaderT Config) a
  liftWith = defaultLiftWith ClubcastT unClubcastT
  restoreT = defaultRestoreT ClubcastT

type Clubcast = ClubcastT IO

runClubcast :: Clubcast a -> IO a
runClubcast cast = do
  mgr <- newManager
  runClubcastWith mgr cast

runClubcastWith :: Manager -> Clubcast a -> IO a
runClubcastWith mgr =
  flip runReaderT (Config mgr 5) . unClubcastT

data Output = Output { getPodcasts :: [Podcast] }
  deriving (Show, Generic)

instance ToJSON Output

data Podcast = Podcast
  { podcastTitle :: Maybe Text
  , podcastArtist :: Maybe Text
  , podcastSummary :: Maybe Text
  , podcastImage :: Maybe Text
  , podcastEpisodes :: [Episode]
  } deriving (Show, Generic)

instance Default Podcast where
  def = Podcast
    { podcastTitle = Nothing
    , podcastArtist = Nothing
    , podcastSummary = Nothing
    , podcastImage = Nothing
    , podcastEpisodes = []
    }

instance ToJSON Podcast where
  toJSON o = object [ "title" .= podcastTitle o
                    , "artist" .= podcastArtist o
                    , "summary" .= podcastSummary o
                    , "image" .= podcastImage o
                    , "episodes" .= podcastEpisodes o
                    ]


data Episode = Episode
  { episodeTitle :: Maybe Text
  , episodeAuthor :: Maybe Text
  , episodeDate :: Maybe Text
  , episodeDuration :: Maybe NominalDiffTime
  , episodeImage :: Maybe Text
  , episodeURL :: Maybe Text
  , episodeDescription :: Maybe Text
  , episodeGuid :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON Episode where
  toJSON o = object [ "title" .= episodeTitle o
                    , "author" .= episodeAuthor o
                    , "date" .= episodeDate o
                    , "duration" .= episodeDuration o
                    , "image" .= episodeImage o
                    , "url" .= episodeURL o
                    , "description" .= episodeDescription o
                    , "guid" .= episodeGuid o
                    ]

instance Default Episode where
  def = Episode
    { episodeTitle = Nothing
    , episodeAuthor = Nothing
    , episodeDate = Nothing
    , episodeDuration = Nothing
    , episodeImage = Nothing
    , episodeURL = Nothing
    , episodeDescription = Nothing
    , episodeGuid = Nothing
    }

data ClubcastException
  = BadResponse Status
  | BadURL String
  | DownloadError HttpException

instance Exception ClubcastException

instance Show ClubcastException where
  show (BadResponse code) =
    "Received a non-200 status code. The code was " <> show code
  show (BadURL url) =
    "Unable to parse URL: " <> show url
  show (DownloadError e) =
    "Failed to download feed information. The HTTP error was " <> show e

isDownloadError :: ClubcastException -> Bool
isDownloadError (DownloadError _) = True
isDownloadError _ = False

isBadURL :: ClubcastException -> Bool
isBadURL (BadURL _) = True
isBadURL _ = False

isBadResponse :: ClubcastException -> Bool
isBadResponse (BadResponse _) = True
isBadResponse _ = False

