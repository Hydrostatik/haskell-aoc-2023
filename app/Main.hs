module Main where

import DayOne
import DayTwo

main :: IO ()
main = do
  dayOneInput <- readFile "app/input/DayOne.txt"
  print $ calculateSumOfAllCalibrationValues dayOneInput
  print $ calculateSumOfAllCalibrationValues' dayOneInput
  dayTwoInput <- readFile "app/input/DayTwo.txt"
  print $ sumOfValidGameIDs dayTwoInput
  print $ productOfMinimumCubesRequired dayTwoInput