{-# LANGUAGE FlexibleContexts #-}

module Clubcast.Downloader where

import Clubcast.Exceptions
import Clubcast.Parser

import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Monoid
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status

saveFile :: ( MonadCatch m
            , MonadReader Manager m
            , MonadResource m) => String -> FilePath -> m ()
saveFile url outputPath = do
  req <- buildRequest url
  mgr <- ask
  resp <- http req mgr
  responseBody resp $$+- CB.sinkFile outputPath

retry :: (Num n, Ord n, Show n) => n -> IO a -> IO a
retry n action =
  action `catch` \e ->
    if n > 0
      then do
        putStrLn $ "Download failed. " <> show n <> " attempts remain."
        retry (n - 1) action
      else throwM . FailedToDownload $ e

getURL :: ( MonadCatch m
          , MonadIO m
          , MonadReader Manager m) => String -> m ByteString
getURL url = do
  req <- buildRequest url
  mgr <- ask
  resp <- httpLbs req mgr `catch` (throwM . FailedToDownload)
  case statusCode . responseStatus $ resp of
    200 ->
      return . responseBody $ resp
    _ -> throwM (BadResponse (responseStatus resp))