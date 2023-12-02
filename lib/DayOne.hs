module DayOne where

import Data.Char
import Data.List

{-
  Part One:

  As they're making final adjustments to the trebuchet, they discover that their calibration
  document has been amended by a very young Elf who was excited to show off her art skills.

  The newly-improved calibration document consists of lines of text; each line originally
  contained a specific calibration value that the Elves now need to recover. On each line,
  the calibration value can be found by combining the first digit and the last digit to form
  a single two-digit-number

  What is the sum of all the calibration values?
-}
calculateSumOfAllCalibrationValues :: String -> Int
calculateSumOfAllCalibrationValues x = sum . map parseCalibrationInput $ lines x

parseCalibrationInput :: String -> Int
parseCalibrationInput = read . (\x -> [head x, last x]) . filter isNumber

{-
  Part Two:

  Your calculation is not quite right... Looks like some of the digits are spelled out with letters (one through nine).
  Find the sum with the corrected calibration algorithm
-}
calculateSumOfAllCalibrationValues' :: String -> Int
calculateSumOfAllCalibrationValues' x = sum . fmap (parseCalibrationInput . parseSpelledOutDigits) $ lines x

parseSpelledOutDigits :: String -> String
parseSpelledOutDigits x =
  foldr
    (\(x, y) acc -> replace x y acc)
    x
    [ ("one", "1"),
      ("two", "2"),
      ("three", "3"),
      ("four", "4"),
      ("five", "5"),
      ("six", "6"),
      ("seven", "7"),
      ("eight", "8"),
      ("nine", "9")
    ]

replace :: String -> String -> String -> String
replace original new whole@(x : y : xs) =
  if original `isPrefixOf` whole
    then replace original new (x : new <> xs)
    else x : replace original new (y : xs)
replace _ _ [x] = [x]
replace _ _ [] = []