{-# LANGUAGE OverloadedStrings #-}

module ClubcastSpec where

import Clubcast
import Control.Concurrent.MVar
import Data.Char
import Data.Text.Lazy (Text)
import Network.HTTP.Conduit
import Test.Hspec
import Text.HTML.TagSoup

spec :: Spec
spec = do
  describe "example tests" $
    it "can use expectations" $
      map toLower "HI" `shouldBe` "hi"

  describe "downloader" $ do

    let maxAttempts = 3

    it "throws an exception after exceeding the limit for failed attempts" $ do

      attemptsMade <- newMVar 0

      retryDownload maxAttempts (countAttempts attemptsMade) `shouldThrow` isDownloadError

      attemptsMade' <- readMVar attemptsMade
      attemptsMade' `shouldBe` maxAttempts


  describe "parsing" $ do

    describe "property extraction" $ do
      it "can extract information from valid tags" $
        getProperty "test" (goodTag "test") `shouldBe` Just "test"
      it "returns Nothing for invalid tags" $
        getProperty "test" (badTag "test") `shouldBe` Nothing
      it "returns Nothing when the tag doesn't match" $
        getProperty "tag a" (goodTag "tag b") `shouldBe` Nothing

    describe "attribute extraction" $ do
      it "can get an href from an image tag" $
        getAttribute "image" "href" imgTag `shouldBe` Just "image-address"
      it "fails to get a nonexisting attribute" $
        getAttribute "image" "href" (badTag "test") `shouldBe` Nothing

    describe "duration parsing" $ do
      it "parses duration in the format MM:SS" $
        getDuration "00:00" `shouldBe` Just 0
      it "parses duration in the format HH:MM:SS" $
        getDuration "00:00:00" `shouldBe` Just 0
      it "converts hours to seconds" $
        getDuration "01:00:00" `shouldBe` Just 3600
      it "converts minutes to seconds" $
        getDuration "01:00" `shouldBe` Just 60
      it "returns Nothing when given invalid input" $
        getDuration "" `shouldBe` Nothing

goodTag :: Text -> [Tag Text]
goodTag tagName =
  [ TagOpen tagName []
  , TagText tagName
  , TagClose tagName
  ]

badTag :: Text -> [Tag Text]
badTag tagName =
  [ TagOpen tagName []
  , TagClose tagName
  ]

imgTag :: [Tag Text]
imgTag = [TagOpen "image" [("href","image-address")]]

countAttempts :: MVar Int -> IO Request
countAttempts a = do
  modifyMVar_ a (\n -> return $ n + 1)
  parseUrlThrow "invalid URL"