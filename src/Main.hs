module Main where

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Monoid
import           Network.HTTP.Simple
import           Text.HTML.TagSoup


data Podcast = Podcast
  { podcastTitle :: Text
  , description :: Text
  , artURL :: Maybe Text
  } deriving (Show)

data Episode = Episode
  { episodeTitle :: Text
  , date :: Text
  , summary :: Text
  , mediaURL :: Text
  , imageURL :: Maybe Text
  } deriving (Show)

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

getPodcastInfo :: IO ()
getPodcastInfo =
  LBS.unpack <$> openURL "http://www.galexmusic.com/podcast/gareth.xml"
  >>= return . parseTags
  >>= mapM_ (putStrLn . show)
  where
    getEpisodes = undefined

main :: IO ()
main = do
  putStrLn "hello world"

