{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Clubcast.Downloader where

import Clubcast.Episode
import Clubcast.Parser
import Clubcast.Types

import           Control.Concurrent
import           Control.Concurrent.STM hiding (retry)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Monoid
import qualified Data.Text.Lazy as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Status
import           System.Directory

saveFile :: String -> FilePath -> Clubcast ()
saveFile url outputPath = do
  req <- buildRequest url
  mgr <- asks manager
  runResourceT $ do
    resp <- http req mgr
    responseBody resp $$+- CB.sinkFile outputPath

retryDownload :: Int -> Clubcast a -> Clubcast a
retryDownload =
  retryTimes (\n -> "Download failed. " <> show (n - 1) <> " attempts remain.") (throwM . DownloadError)

retryTimes :: Exception e => (Int -> String) -> (e -> Clubcast a) -> Int -> Clubcast a -> Clubcast a
retryTimes msg failAction n action =
  action `catch` \e ->
    if n > 1
      then do
        liftIO . putStrLn $ msg n
        retryTimes msg failAction (n - 1) action
      else failAction e

getURL :: String -> Clubcast ByteString
getURL url = do
  req <- buildRequest url
  mgr <- asks manager
  resp <- httpLbs req mgr `catch` (throwM . DownloadError)
  case statusCode . responseStatus $ resp of
    200 ->
      return . responseBody $ resp
    _ -> throwM (BadResponse (responseStatus resp))

makeQueue :: MonadIO m => Int -> [Episode] -> ClubcastT m (TQueue (Episode, String), TQueue Episode)
makeQueue workerCount episodes = do

  (q,r) <- liftIO . atomically $
    (,) <$> newTQueue <*> newTQueue

  liftIO . atomically $ forM_ episodes $ \e ->
    case episodeURL e of
      Just url ->
        writeTQueue q (e, T.unpack url)
      _ -> return ()


  _ <- createWorkers workerCount q r

  return (q, r)

createWorkers :: MonadIO m => Int -> TQueue (Episode, String) -> TQueue Episode -> ClubcastT m [ThreadId]
createWorkers count jobQueue resultQueue = do
  mgr <- asks manager
  replicateM count . liftIO . forkIO $
    runClubcastWith mgr $ worker jobQueue resultQueue

urlToOutput :: String -> String
urlToOutput url =
  "output/" <> (reverse . takeWhile (/= '/') . reverse $ url)

worker :: TQueue (Episode, String) -> TQueue Episode -> Clubcast ()
worker jobQueue resultQueue =
  forever $ do
    nextItem <- liftIO . atomically $ tryReadTQueue jobQueue
    case nextItem of
      Just (associatedEp, url) -> do

        let output = urlToOutput url
        fileExists <- liftIO . doesFileExist $ output

        unless fileExists
          $ retryDownload 3
          $ saveFile url output

        --extract track list
        liftIO . atomically $
          writeTQueue resultQueue
            (addTrackList associatedEp ["Track 1", "Track 2", "Track 3", "Track 4"])

        --remove file
        liftIO $
          removeFile output

      Nothing -> liftIO $ do
        tid <- myThreadId
        killThread tid

