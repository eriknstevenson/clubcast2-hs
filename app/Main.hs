module Main where

import           Clubcast
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import           GHC.IO.Encoding (setLocaleEncoding, utf8)
import           Network.HTTP.Client.Conduit

main :: IO ()
main = do
  setLocaleEncoding utf8
  output <- T.decodeUtf8 . encode <$> (withManager . runResourceT $ mapM getFeedInfo podcasts)
  T.writeFile "output/data.json" output

podcasts :: [String]
podcasts =
  [ "http://www.galexmusic.com/podcast/gareth.xml"
  , "http://podcast.djhardwell.com/podcast.xml"
  ]
