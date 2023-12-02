module Main where

import DayOne
import DayTwo
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day One" $ do
    it "Calculate the sum of the parsed calibration values" $ do
      calculateSumOfAllCalibrationValues "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet" `shouldBe` 142
    it "Calculate the sum of the parsed calibration values with stringified digits" $ do
      calculateSumOfAllCalibrationValues' "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen" `shouldBe` 281
  describe "Day Two" $ do
    it "Find the sum of all valid game ids" $ do
      sumOfValidGameIDs "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n" `shouldBe` 8
    it "Find the sum of the product of the minimum number of cubes required to play every game" $ do
      productOfMinimumCubesRequired "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n" `shouldBe` 2286