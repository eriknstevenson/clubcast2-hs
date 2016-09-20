{-# LANGUAGE OverloadedStrings #-}

module Clubcast.Episode where

import Clubcast.Parser
import Clubcast.Types
import Control.Applicative
import Data.Text.Lazy (Text)
import Text.HTML.TagSoup

makeEpisode :: Episode -> [Tag Text] -> Episode
makeEpisode fallback itemContents = Episode
  { episodeTitle = getProperty "title" itemContents
  , episodeAuthor = getProperty "itunes:author" itemContents <|>
                    episodeAuthor fallback
  , episodeDate = getProperty "pubDate" itemContents
  , episodeGuid = getProperty "guid" itemContents
  , episodeDuration =
      getProperty "itunes:duration" itemContents >>= getDuration
  , episodeImage = getAttribute "image" "href" itemContents <|>
                   episodeImage fallback
  , episodeURL = getProperty "link" itemContents <|>
                 getAttribute "enclosure" "url" itemContents
  , episodeDescription =
      getProperty "description" itemContents <|>
      getProperty "itunes:summary" itemContents
  , episodeTrackList = []
  }

addTrackList :: Episode -> [Text] -> Episode
addTrackList ep tracks = ep {episodeTrackList = tracks}
