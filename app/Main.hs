module Main where

import Clubcast
import Control.Monad.Reader
import Data.Aeson
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.HTTP.Client

main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  output <- T.decodeUtf8 . encode <$> runReaderT (mapM getFeedInfo podcasts) mgr
  T.writeFile "output/data.json" output

podcasts :: [String]
podcasts =
  [ "http://www.galexmusic.com/podcast/gareth.xml"
  , "http://podcast.djhardwell.com/podcast.xml"
  ]
