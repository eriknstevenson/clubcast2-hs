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

saveFile :: ( MonadCatch m
            , MonadReader Manager m
            , MonadResource m) => String -> FilePath -> m ()
saveFile url outputPath = do
  req <- buildRequest url
  mgr <- ask
  resp <- http req mgr
  responseBody resp $$+- CB.sinkFile outputPath

retryDownload :: ( MonadCatch m
                 , MonadReader Manager m
                 , MonadResource m) => Int -> m a -> m a
retryDownload =
  retry (\n -> "Download failed. " <> show (n - 1) <> " attempts remain.") (throwM . DownloadError)

retry :: ( Exception e
         , MonadCatch m
         , MonadReader Manager m
         , MonadResource m)
         => (Int -> String) -> (e -> m a) -> Int -> m a -> m a
retry msg failAction n action =
  action `catch` \e ->
    if n > 1
      then do
        liftIO . putStrLn $ msg n
        retry msg failAction (n - 1) action
      else failAction e

getURL :: ( MonadCatch m
          , MonadIO m
          , MonadReader Manager m) => String -> m ByteString
getURL url = do
  req <- buildRequest url
  mgr <- ask
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

makeDownloadQueue :: MonadIO m => m (TQueue String)
makeDownloadQueue =
  liftIO . atomically $ newTQueue

createWorkers :: ( MonadCatch m
                 , MonadIO m
                 , MonadResource m
                 , MonadReader Manager m) => Int -> TQueue String -> m ()
createWorkers count jobList = do
  mgr <- ask
  replicateM_ count . liftIO . forkIO $
    runReaderT (runResourceT $ worker jobList) mgr

worker :: ( MonadCatch m
          , MonadIO m
          , MonadReader Manager m
          , MonadResource m) => TQueue String -> m ()
worker jobList =
  forever $ do
    url <- liftIO . atomically $ readTQueue jobList

    let output = "output/" <> (reverse . takeWhile (/= '/') . reverse $ url)

    fileExists <- liftIO . doesFileExist $ output

    unless fileExists $
      retryDownload 3 $ saveFile url output

addJob :: MonadIO m => TQueue String -> String -> m ()
addJob downloadQueue url =
  liftIO . atomically $ writeTQueue downloadQueue url