{-# LANGUAGE OverloadedStrings #-}

module Clubcast.Episode where

import Clubcast.Parser
import Clubcast.Types

import           Control.Applicative
import           Data.Text.Lazy (Text)
import           Text.HTML.TagSoup

{-
downloadEpisode :: ( MonadCatch m
                   , MonadReader Manager m
                   , MonadResource m) => Episode -> m ()
downloadEpisode ep =
  case T.unpack <$> episodeURL ep of
    Just url -> do
      let output = "output/" <> (reverse . takeWhile (/= '/') . reverse $ url)
      fileExists <- liftIO . doesFileExist $ output
      unless fileExists $ do
        liftIO . putStrLn $ "Downloading: " <> url
        retryDownload 3 $ saveFile url output

    Nothing -> liftIO . putStrLn $ "No URL for " <> show ep
-}

makeEpisode :: [Tag Text] -> Episode
makeEpisode itemContents = Episode
  { episodeTitle = getProperty "title" itemContents
  , episodeAuthor = getProperty "itunes:author" itemContents
  , episodeDate = getProperty "pubDate" itemContents
  , episodeGuid = getProperty "guid" itemContents
  , episodeDuration =
      getProperty "itunes:duration" itemContents >>= getDuration
  , episodeImage = getAttribute "image" "href" itemContents
  , episodeURL = getProperty "link" itemContents <|>
                 getAttribute "enclosure" "url" itemContents
  , episodeDescription =
      getProperty "description" itemContents <|>
      getProperty "itunes:summary" itemContents
  }
