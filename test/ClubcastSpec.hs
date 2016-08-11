{-# LANGUAGE OverloadedStrings #-}

module ClubcastSpec where

import Clubcast
import Data.Char
import Test.Hspec
import Text.HTML.TagSoup

spec :: Spec
spec = do
  describe "example tests" $ 
    it "can use expectations" $
      map toLower "HI" `shouldBe` "hi"

  describe "podcast feed parsing" $ do

    describe "property extraction" $ do
      it "can extract information from valid tags" $
        getProperty "test" (goodTag "test") `shouldBe` Just "test"
      it "returns Nothing for invalid tags" $
        getProperty "test" (badTag "test") `shouldBe` Nothing
      it "returns Nothing when the tag doesn't match" $
        getProperty "tag a" (goodTag "tag b") `shouldBe` Nothing
    
    describe "duration parsing" $ do
      it "parses duration in the format MM:SS" $
        getDuration "00:00" `shouldBe` Just 0
      it "parses duration in the format HH:MM:SS" $
        getDuration "00:00:00" `shouldBe` Just 0
      it "converts hours to seconds" pending
      it "converts minutes to seconds" pending
      it "returns Nothing when given invalid input" pending

goodTag :: String -> [Tag String]
goodTag tagName = 
  [ TagOpen tagName []
  , TagText tagName
  , TagClose tagName
  ]

badTag :: String -> [Tag String]
badTag tagName = 
  [ TagOpen tagName []
  , TagClose tagName 
  ]
