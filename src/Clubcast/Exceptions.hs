

module Clubcast.Exceptions where

import           Control.Monad.Catch
import           Data.Monoid
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

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
