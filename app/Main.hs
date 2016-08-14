module Main where

import Clubcast
import Control.Monad.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Network.HTTP.Client

main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  output <- UTF8.toString . encode <$> runReaderT (mapM getFeedInfo podcasts) mgr
  writeFile "output/data.json" output

podcasts :: [String]
podcasts =
  [ "http://www.galexmusic.com/podcast/gareth.xml"
  , "http://podcast.djhardwell.com/podcast.xml"
  ]
