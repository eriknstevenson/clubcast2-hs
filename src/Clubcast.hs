{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Clubcast where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Char
import           Data.Default
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Monoid
import           GHC.Generics
import           Network.HTTP.Simple
import           Safe
import           Text.HTML.TagSoup
import           Text.ParserCombinators.ReadP

data Output = Output { getPodcasts :: [Podcast] } deriving (Show, Generic)

instance ToJSON Output

data Podcast = Podcast
  { podcastArtist :: Maybe Text
  , podcastEpisodes :: [Episode]
  , podcastImage :: Maybe Text
  , podcastSummary :: Maybe Text
  , podcastTitle :: Maybe Text
  } deriving (Show, Generic)

instance Default Podcast where
  def = Podcast 
    { podcastArtist = Nothing
    , podcastEpisodes = []
    , podcastImage = Nothing
    , podcastSummary = Nothing
    , podcastTitle = Nothing
    }

instance ToJSON Podcast where
  toJSON o = object [ "title" .= podcastTitle o
                    , "artist" .= podcastArtist o
                    , "image" .= podcastImage o
                    , "summary" .= podcastSummary o
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
      episodes = map makeEpisode (groupsOf "item" tags)
  return $ def {podcastEpisodes = episodes}
    
groupsOf :: String -> [Tag String] -> [[Tag String]]
groupsOf str =
  map (takeWhile (~/= TagClose str) . tail) . sections (~== TagOpen str [])
   
makeEpisode :: [Tag String] -> Episode
makeEpisode itemContents = Episode
  { episodeTitle = getProperty "title" itemContents
  , episodeAuthor = getProperty "itunes:author" itemContents
  , episodeDate = getProperty "pubDate" itemContents
  , episodeGuid = getProperty "guid" itemContents
  , episodeDuration = 
      getProperty "itunes:duration" itemContents >>= getDuration
  , episodeImage = getAttribute "image" "href" itemContents
  , episodeURL = getProperty "link" itemContents
  , episodeDescription = 
      getProperty "description" itemContents <|> 
      getProperty "itunes:summary" itemContents
  }

getDuration :: Text -> Maybe NominalDiffTime
getDuration = 
  fmap (fromInteger . fst) . headMay . parser . T.unpack
  where
    parser = readP_to_S (hhmmss <|> mmss)

    hhmmss = 
      liftA2 (+) (hours <* char ':') mmss

    mmss =
      liftA2 (+) (minutes <* char ':') (seconds <* eof)

    hours = fmap (* 3600) $ twoDigits
    minutes = fmap (* 60) $ twoDigits
    seconds = twoDigits
    twoDigits = fmap read $ count 2 (satisfy isDigit)

getProperty :: String -> [Tag String] -> Maybe Text
getProperty field = 
  fmap T.pack . maybeTagText <=< headMay <=< tailMay . dropWhile (~/= TagOpen field [])

getAttribute :: String -> String -> [Tag String] -> Maybe Text
getAttribute field attr tags =
  case headMay . dropWhile (not . isTagOpenName field) $ tags of
    Just tag@(TagOpen _ _) ->
      case fromAttrib attr tag of
        "" -> Nothing
        contents -> 
          return . T.pack $ contents 
    _ -> Nothing


