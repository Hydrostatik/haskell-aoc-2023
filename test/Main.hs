module Main (main) where

import DayOne
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day One Task One" $ do
    it "Calculate the sum of the parsed calibration values" $ do
      calculateSumOfAllCalibrationValues "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet" `shouldBe` 142
    it "Calculate the sum of the parsed calibration values with stringified digits" $ do
      calculateSumOfAllCalibrationValues' "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen" `shouldBe` 281