module Main where

import           Clubcast
import           Data.Aeson
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import           GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do
  setLocaleEncoding utf8

  output <- T.decodeUtf8 . encode <$> (runClubcast doStuff)

  T.writeFile "output/data.json" output

  return ()

podcasts :: [String]
podcasts =
  [ "http://www.galexmusic.com/podcast/gareth.xml"
  , "http://podcast.djhardwell.com/podcast.xml"
  ]

doStuff :: Clubcast [Podcast]
doStuff = mapM getFeedInfo podcasts
