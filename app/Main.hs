module Main where

import Control.Exception
import DayEight (stepsUntilAllDestinationsReached, stepsUntilDestinationReached)
import DayFive
import DayFour
import DayOne
import DaySeven
import DaySix
import DayThree
import DayTwo
import System.CPUTime
import Text.Printf

time :: IO ()
time = do
  start <- getCPUTime
  dayFiveInput <- readFile "app/Input/DayFive.txt"
  print $ lowestLocationNumber' dayFiveInput
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ 12)
  printf "Computation time: %0.9f sec\n" (diff :: Double)

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
  daySixInput <- readFile "app/input/DaySix.txt"
  print $ productOfAllWaysToWin daySixInput
  print $ waysToWinBigRace daySixInput
  daySevenInput <- readFile "app/input/DaySeven.txt"
  print $ totalWinnings daySevenInput
  print $ totalWinnings' daySevenInput
  dayEightInput <- readFile "app/input/DayEight.txt"
  print $ stepsUntilDestinationReached dayEightInput
  print $ stepsUntilAllDestinationsReached dayEightInput