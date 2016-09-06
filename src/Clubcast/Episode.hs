{-# LANGUAGE OverloadedStrings #-}

module Clubcast.Episode where

import Clubcast.Parser
import Clubcast.Types
import Control.Applicative
import Data.Text.Lazy (Text)
import Text.HTML.TagSoup

makeEpisode :: Maybe Text -> [Tag Text] -> Episode
makeEpisode defaultImage itemContents = Episode
  { episodeTitle = getProperty "title" itemContents
  , episodeAuthor = getProperty "itunes:author" itemContents
  , episodeDate = getProperty "pubDate" itemContents
  , episodeGuid = getProperty "guid" itemContents
  , episodeDuration =
      getProperty "itunes:duration" itemContents >>= getDuration
  , episodeImage = getAttribute "image" "href" itemContents <|> defaultImage
  , episodeURL = getProperty "link" itemContents <|>
                 getAttribute "enclosure" "url" itemContents
  , episodeDescription =
      getProperty "description" itemContents <|>
      getProperty "itunes:summary" itemContents
  }
