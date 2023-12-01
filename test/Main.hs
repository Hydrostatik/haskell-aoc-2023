module Main (main) where

import DayOne
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day One Task One" $ do
    it "Placeholder text on behavior" $ do
      "comparison" `shouldBe` "comparison"