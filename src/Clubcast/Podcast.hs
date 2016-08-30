{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Clubcast.Podcast where

import Clubcast.Downloader
import Clubcast.Episode
import Clubcast.Parser

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Default
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           GHC.Generics
import           Network.HTTP.Conduit
import           Text.HTML.TagSoup

data Output = Output { getPodcasts :: [Podcast] } deriving (Show, Generic)

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

getFeedInfo :: ( MonadIO m
               , MonadCatch m
               , MonadReader Manager m
               , MonadResource m) => TQueue String -> String -> m Podcast
getFeedInfo downloadQueue url = do
  resp <- T.decodeUtf8 <$> getURL url
  let tags = parseTags resp
      episodes = map makeEpisode (groupsOf "item" tags)
      artist = getProperty "itunes:author" tags
      image = getAttribute "itunes:image" "href" tags
      title = getProperty "title" tags
      summary = getProperty "description" tags     <|>
                getProperty "itunes:subtitle" tags <|>
                getProperty "itunes:summary" tags

  forM_ episodes $ \e ->
    case episodeURL e of
      Just downloadURL -> addJob downloadQueue (T.unpack downloadURL)
      Nothing -> return ()

  return Podcast { podcastEpisodes = episodes
                 , podcastArtist = artist
                 , podcastImage = image
                 , podcastTitle = title
                 , podcastSummary = summary
                 }
