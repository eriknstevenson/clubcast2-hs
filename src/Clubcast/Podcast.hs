{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Clubcast.Podcast where

import Clubcast.Downloader
import Clubcast.Episode
import Clubcast.Parser
import Clubcast.Types

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Default
import qualified Data.Text.Lazy.Encoding as T
import           Text.HTML.TagSoup

getFeedInfo :: String -> Clubcast Podcast
getFeedInfo url = do
  resp <- T.decodeUtf8 <$> getURL url
  let tags = parseTags resp
      image = getAttribute "itunes:image" "href" tags
      fallback = def { episodeImage = image
                     , episodeAuthor = artist}
      artist = getProperty "itunes:author" tags
      title = getProperty "title" tags
      summary = getProperty "description" tags     <|>
                getProperty "itunes:subtitle" tags <|>
                getProperty "itunes:summary" tags

  episodes <- do

    let itemList = map (makeEpisode fallback) (groupsOf "item" tags)

    (jobQueue, resultQueue) <- makeQueue 5 itemList

    liftIO $ do
      atomically $ do
        isDone <- isEmptyTQueue jobQueue
        unless isDone retry

      tQueueToList resultQueue

  return Podcast { podcastEpisodes = episodes
                 , podcastArtist = artist
                 , podcastImage = image
                 , podcastTitle = title
                 , podcastSummary = summary
                 }

tQueueToList :: TQueue a -> IO [a]
tQueueToList q = reverse <$> tQueueToList' []
  where
    tQueueToList' acc = do
      tryRead <- atomically $ tryReadTQueue q
      case tryRead of
        Just x -> tQueueToList' (x:acc)
        Nothing -> return acc
