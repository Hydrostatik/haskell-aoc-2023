module Main where

import DayFive (lowestLocationNumber, lowestLocationNumber')
import DayFour (totalNumberOfScratchCards, totalWorthOfScratchCards)
import DayOne
import DayThree
import DayTwo

main :: IO ()
main = do
  dayOneInput <- readFile "app/input/DayOne.txt"
  print $ calculateSumOfAllCalibrationValues dayOneInput
  print $ calculateSumOfAllCalibrationValues' dayOneInput
  dayTwoInput <- readFile "app/input/DayTwo.txt"
  print $ sumOfValidGameIDs dayTwoInput
  print $ productOfMinimumCubesRequired dayTwoInput
  dayThreeInput <- readFile "app/input/DayThree.txt"
  print $ findSumOfAllPartNumbers dayThreeInput
  print $ sumOfAllGearRatios dayThreeInput
  dayFourInput <- readFile "app/Input/DayFour.txt"
  print $ totalWorthOfScratchCards dayFourInput
  print $ totalNumberOfScratchCards dayFourInput
  dayFiveInput <- readFile "app/Input/DayFive.txt"
  print $ lowestLocationNumber dayFiveInput
  print $ lowestLocationNumber' dayFiveInput