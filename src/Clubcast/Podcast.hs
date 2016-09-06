{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Clubcast.Podcast where

import Clubcast.Downloader
import Clubcast.Episode
import Clubcast.Parser
import Clubcast.Types

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Text.HTML.TagSoup

getFeedInfo :: TQueue String -> String -> Clubcast Podcast
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
