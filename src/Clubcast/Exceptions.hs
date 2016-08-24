

module Clubcast.Exceptions where

import           Control.Monad.Catch
import           Data.Monoid
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

data ClubCastException
  = BadResponse Status
  | BadURL String
  | DownloadError HttpException

instance Exception ClubCastException

instance Show ClubCastException where
  show (BadResponse code) =
    "Received a non-200 status code. The code was " <> show code
  show (BadURL url) =
    "Unable to parse URL: " <> show url
  show (DownloadError e) =
    "Failed to download feed information. The HTTP error was " <> show e

isDownloadError (DownloadError _) = True
isDownloadError _ = False

isBadURL (BadURL _) = True
isBadURL _ = False

isBadResponse (BadResponse _) = True
isBadResponse _ = False