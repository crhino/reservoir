module Main where

import Reservoir
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Validate hello function" $ do
    it "hello is supposed to prefix Hello! to things" $ do
      hello "me" `shouldBe` ("Hello, me" :: String)
