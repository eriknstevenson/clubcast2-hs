module Main where

import Clubcast
import Data.Aeson
import Data.ByteString.Lazy.Char8 as LBS

main :: IO ()
main = do
  output <- mapM getPodcast podcasts
  LBS.writeFile "output/data.json" $ encode output

podcasts :: [String]
podcasts =
  [ "http://www.galexmusic.com/podcast/gareth.xml"
  , "http://podcast.djhardwell.com/podcast.xml"
  ]
