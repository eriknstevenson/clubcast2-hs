{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Monoid
import           GHC.Generics
import           Network.HTTP.Simple
import           Safe
import           Text.HTML.TagSoup

data Output = Output { podcasts :: [Podcast] } deriving (Show, Generic)

instance ToJSON Output

data Podcast = Podcast
  { podcastArtist :: Text
  , podcastEpisodes :: [Episode]
  , podcastImage :: Maybe Text
  , podcastSummary :: Text
  , podcastTitle :: Text
  } deriving (Show, Generic)

instance Default Podcast where
  def = Podcast 
    { podcastArtist = ""
    , podcastEpisodes = []
    , podcastImage = Nothing
    , podcastSummary = ""
    , podcastTitle = ""
    }

instance ToJSON Podcast where
  toJSON o = object [ "title" .= podcastTitle o
                    , "artist" .= podcastArtist o
                    , "image" .= podcastImage o
                    , "summary" .= podcastSummary o
                    , "episodes" .= podcastEpisodes o
                    ]

data Episode = Episode
  { episodeTitle :: Text
  , episodeAuthor :: Text
  , episodeDate :: Text
  , episodeDuration :: NominalDiffTime
  , episodeImage :: Maybe Text
  , episodeURL :: Text
  , episodeDescription :: Text
  , episodeGuid :: Text
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

--TODO: Use OverloadedStrings extension
instance Default Episode where
  def = Episode 
    { episodeTitle = ""
    , episodeAuthor = ""
    , episodeDate = ""
    , episodeDuration = 0
    , episodeImage = Nothing 
    , episodeURL = ""
    , episodeDescription = ""
    , episodeGuid = ""
    }

data ClubCastException
  = BadResponse Int
  | BadURL String
  | FailedToDownload HttpException

instance Exception ClubCastException

instance Show ClubCastException where
  show (BadResponse code) =
    "Received a non-200 status code. The code was " <> show code
  show (BadURL url) =
    "Unable to parse URL: " <> show url
  show (FailedToDownload e) =
    "Failed to download feed information. The HTTP error was " <> show e

openURL :: ( MonadCatch m
           , MonadIO m ) => String -> m LBS.ByteString
openURL url = do
  req <- buildRequest url
  resp <- catch (httpLBS req) (throwM . FailedToDownload)
  case getResponseStatusCode resp of
    200 ->
      return . getResponseBody $ resp
    code -> throwM (BadResponse code)

buildRequest :: (MonadCatch m) => String -> m Request
buildRequest url =
  catchAll (parseRequest url) (\_ -> throwM $ BadURL url)

getPodcast :: String -> IO Podcast
getPodcast url = do
  resp <- LBS.unpack <$> openURL url
  let tags = parseTags resp
      episodes = map getEpisode (getItems tags)
  return $ def {podcastEpisodes = episodes}
  where
    groupOf :: String -> [Tag String] -> [[Tag String]]
    groupOf str =
      map (takeWhile (~/= TagClose str) . tail) . sections (~== TagOpen str [])
   
    getItems = groupOf "item"
    
    getEpisode :: [Tag String] -> Episode
    getEpisode tags = def
      { episodeTitle = fromMaybe "" $ getProperty "title" tags
      , episodeAuthor = fromMaybe "" $ getProperty "itunes:author" tags
      , episodeDate = fromMaybe "" $ getProperty "pubDate" tags
      , episodeGuid = fromMaybe "" $ getProperty "guide" tags
      }

getProperty :: String -> [Tag String] -> Maybe Text
getProperty field = 
  fmap T.pack . maybeTagText <=< headMay <=< tailMay . dropWhile (~/= TagOpen field [])
    
main :: IO ()
main = do
  output <- mapM getPodcast [electricForLife, hardwellOnAir]
  LBS.writeFile "output/data.json" $ encode output

electricForLife :: String
electricForLife = "http://www.galexmusic.com/podcast/gareth.xml"

hardwellOnAir :: String
hardwellOnAir = "http://podcast.djhardwell.com/podcast.xml"
