{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Clubcast.Episode where

import Clubcast.Downloader
import Clubcast.Exceptions
import Clubcast.Parser

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Default
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time.Clock
import           Data.Monoid
import           GHC.Generics
import           Network.HTTP.Conduit
import           System.Directory
import           Text.HTML.TagSoup

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
        saveFile url output `catch` (throwM . FailedToDownload)

    Nothing -> liftIO . putStrLn $ "No URL for " <> show ep


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
