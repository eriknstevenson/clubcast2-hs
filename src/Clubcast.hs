{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Clubcast where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Char
import           Data.Default
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T
import           Data.Time.Clock
import           Data.Monoid
import           GHC.Generics
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status
import           Safe
import           Text.HTML.TagSoup
import           Text.ParserCombinators.ReadP

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
  = BadResponse Status
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

getURL :: ( MonadCatch m
          , MonadIO m
          , MonadReader Manager m) => String -> m ByteString
getURL url = do
  req <- buildRequest url
  mgr <- ask
  resp <- catch (httpLbs req mgr) (throwM . FailedToDownload)
  case statusCode . responseStatus $ resp of
    200 ->
      return . responseBody $ resp
    _ -> throwM (BadResponse (responseStatus resp))

buildRequest :: (MonadCatch m) => String -> m Request
buildRequest url =
  catchAll (parseRequest url) (\_ -> throwM $ BadURL url)

getFeedInfo :: ( MonadIO m
               , MonadCatch m
               , MonadReader Manager m) => String -> m Podcast
getFeedInfo url = do
  resp <- T.decodeUtf8 <$> getURL url
  let tags = parseTags resp
      episodes = map makeEpisode (groupsOf "item" tags)
      artist = getProperty "itunes:author" tags
      image = getAttribute "itunes:image" "href" tags
      title = getProperty "title" tags
      summary = getProperty "description" tags     <|>
                getProperty "itunes:subtitle" tags <|>
                getProperty "itunes:summary" tags

  return $ Podcast { podcastEpisodes = episodes
                   , podcastArtist = artist
                   , podcastImage = image
                   , podcastTitle = title
                   , podcastSummary = summary
                   }

groupsOf :: Text -> [Tag Text] -> [[Tag Text]]
groupsOf str =
  map (takeWhile (~/= TagClose str) . tail) . sections (~== TagOpen str [])

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

getDuration :: Text -> Maybe NominalDiffTime
getDuration =
  fmap (fromInteger . fst) . headMay . parser . T.unpack
  where
    parser = readP_to_S (hhmmss <|> mmss)

    hhmmss =
      liftA2 (+) (hours <* char ':') mmss

    mmss =
      liftA2 (+) (minutes <* char ':') (seconds <* eof)

    hours = (* 3600) <$> twoDigits
    minutes = (* 60) <$> twoDigits
    seconds = twoDigits
    twoDigits = read <$> count 2 (satisfy isDigit)

getProperty :: Text -> [Tag Text] -> Maybe Text
getProperty field =
  maybeTagText <=< headMay <=< tailMay . dropWhile (~/= TagOpen field [])

getAttribute :: Text -> Text -> [Tag Text] -> Maybe Text
getAttribute field attr tags =
  case headMay . dropWhile (not . isTagOpenName field) $ tags of
    Just tag@(TagOpen _ _) ->
      case fromAttrib attr tag of
        "" -> Nothing
        contents ->
          return contents
    _ -> Nothing

