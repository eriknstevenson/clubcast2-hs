{-# LANGUAGE FlexibleContexts #-}

module Clubcast.Downloader where

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
  retry (\n -> "Download failed. " <> show (n - 1) <> " attempts remain.") (throwM . DownloadError)

retry :: Exception e => (Int -> String) -> (e -> Clubcast a) -> Int -> Clubcast a -> Clubcast a
retry msg failAction n action =
  action `catch` \e ->
    if n > 1
      then do
        liftIO . putStrLn $ msg n
        retry msg failAction (n - 1) action
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

{-doJobs :: IO ()
doJobs = do
  jobs <- atomically newTQueue
  replicateM_ 3 $ forkIO $ worker jobs
  insertSomeJobs jobs
  putStrLn "inserted jobs"
  return ()

insertSomeJobs :: TQueue String -> IO ()
insertSomeJobs jobList = do
  atomically . replicateM_ 30 $ writeTQueue jobList "an URL"
  return ()
-}
-----------

makeDownloadQueue :: MonadIO m => ClubcastT m (TQueue String)
makeDownloadQueue =
  liftIO . atomically $ newTQueue

createWorkers :: MonadIO m => Int -> TQueue String -> ClubcastT m ()
createWorkers count jobList = do
  mgr <- asks manager
  replicateM_ count . liftIO . forkIO $
    runClubcastWith mgr $ worker jobList

worker :: TQueue String -> Clubcast ()
worker jobList =
  forever $ do
    url <- liftIO . atomically $ readTQueue jobList

    let output = "output/" <> (reverse . takeWhile (/= '/') . reverse $ url)

    fileExists <- liftIO . doesFileExist $ output

    unless fileExists $
      retryDownload 3 $ saveFile url output

addJob :: TQueue String -> String -> Clubcast ()
addJob downloadQueue url =
  liftIO . atomically $ writeTQueue downloadQueue url
