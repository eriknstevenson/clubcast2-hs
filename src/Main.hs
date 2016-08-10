{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Monoid
import           GHC.Generics
import           Network.HTTP.Simple
import           Text.HTML.TagSoup

data Podcast = Podcast
  { podcastArtist :: Text
  , podcastEpisodes :: [Episode]
  , podcastImage :: Maybe Text
  , podcastSummary :: Text
  , podcastTitle :: Text
  } deriving (Show, Generic)

instance ToJSON Podcast

data Episode = Episode
  { episodeTitle :: Text
  , episodeAuthor :: Text
  , episodeDate :: Text
  , episodeDuration :: Text
  , episodeImage :: Maybe Text
  , episodeURL :: Text
  , episodeDescription :: Text
  , episodeGuid :: Text
  } deriving (Show, Generic)

instance ToJSON Episode

instance Default Episode where
  def = Episode { episodeTitle = T.pack ""
                , episodeAuthor = T.pack ""
                , episodeDate = T.pack ""
                , episodeDuration = T.pack ""
                , episodeImage = Just $ T.pack ""
                , episodeURL = T.pack ""
                , episodeDescription = T.pack ""
                , episodeGuid = T.pack ""
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

haskellLastModifiedDateTime :: IO ()
haskellLastModifiedDateTime = do
  src <- LBS.unpack <$> openURL "http://wiki.haskell.org/Haskell"
  let lastModifiedDateTime = test $ parseTags src
  putStrLn $ "wiki last updated: " <> lastModifiedDateTime
  where
    fromFooter =
      unwords . drop 6 . words. innerText . take 2 . dropWhile (~/= "<li id=lastmod>")
    test =
      concatMap show . take 2 . dropWhile (~/= "<li id=lastmod>")

getPodcast :: IO ()
getPodcast = do
  resp <- LBS.unpack <$> openURL "http://www.galexmusic.com/podcast/gareth.xml"
  let tags = parseTags resp
      items = getItems tags
      episodes = map getEpisode items
  mapM_ print episodes
  where
    getItems = 
      map (takeWhile (~/= TagClose "item") . tail) . sections (~== TagOpen "item" [])
    getEpisode :: [Tag String] -> Episode
    getEpisode tags =
      def
        { episodeTitle = getTitle tags
        , episodeAuthor = getAuthor tags
        , episodeDate = getDate tags
        , episodeGuid = getGuid tags
        }
    getTitle = getProperty "title" 
    getAuthor = getProperty "itunes:author"
    getGuid = getProperty "guid"
    getDate = getProperty "pubDate" 

getProperty :: String -> [Tag String] -> Text
getProperty field = 
  T.pack . fromTagText . head . tail . dropWhile (~/= TagOpen field [])
    
main :: IO ()
main = getPodcast

