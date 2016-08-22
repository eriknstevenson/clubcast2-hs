{-# LANGUAGE OverloadedStrings #-}

module Clubcast.Parser where

import Clubcast.Exceptions

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Data.Char
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import           Data.Time.Clock
import           Network.HTTP.Conduit
import           Safe
import           Text.HTML.TagSoup
import           Text.ParserCombinators.ReadP

buildRequest :: (MonadCatch m) => String -> m Request
buildRequest url =
  parseRequest url `catchAll` \_ -> throwM $ BadURL url

groupsOf :: Text -> [Tag Text] -> [[Tag Text]]
groupsOf str =
  map (takeWhile (~/= TagClose str) . tail) . sections (~== TagOpen str [])

getDuration :: Text -> Maybe NominalDiffTime
getDuration =
  fmap (fromInteger . fst) . headMay . parser . T.unpack
  where
    parser = readP_to_S (hhmmss <|> mmss)

    hhmmss =
      liftA2 (+) (hours <* char ':') mmss

    mmss =
      liftA2 (+) (minutes <* char ':') (seconds <* eof)

    hours = (* 3600) <$> twoDigits
    minutes = (* 60) <$> twoDigits
    seconds = twoDigits
    twoDigits = read <$> count 2 (satisfy isDigit)

getProperty :: Text -> [Tag Text] -> Maybe Text
getProperty field =
  maybeTagText <=< headMay <=< tailMay . dropWhile (~/= TagOpen field [])

getAttribute :: Text -> Text -> [Tag Text] -> Maybe Text
getAttribute field attr tags =
  case headMay . dropWhile (not . isTagOpenName field) $ tags of
    Just tag@(TagOpen _ _) ->
      case fromAttrib attr tag of
        "" -> Nothing
        contents ->
          return contents
    _ -> Nothing
