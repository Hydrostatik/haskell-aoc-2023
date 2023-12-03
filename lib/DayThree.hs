module DayThree where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

{-
  Part One:

  The gondola lift is broken! The engineer explains that if you can add up all the part numbers
  in the engine schematic, it should be easy to work out which part is missing.

  There are lots of numbers and symbols you don't really understand, but apparantely
  any number adjacent to a symbol, even diagonally, is a "part number" and should be included
  in your sum. (Periods (.) do not count as a symbol.)
-}
findSumOfAllPartNumbers :: String -> Int
findSumOfAllPartNumbers x = sum . mconcat . map validPartChunks . chunks $ lines x

validPartChunks :: [String] -> [Int]
validPartChunks [x, y] =
  map (\x -> fromMaybe 0 (readMaybe x :: Maybe Int)) $
    concatMap
      ( (filter (/= "") . words)
          . map (\(x, _) -> if not $ isDigit x then ' ' else x)
      )
      ( filter
          ( ((not . null) . filter (\x -> not (isDigit x) && x /= '.'))
              . uncurry (<>)
              . unzip
          )
          ( splitWhen
              ( \(x, y) ->
                  not (isDigit x) && x == '.' && y == '.'
              )
              $ zip x y
          )
      )
validPartChunks [x, y, z] =
  map (\x -> fromMaybe 0 (readMaybe x :: Maybe Int)) $
    concatMap
      ( (filter (/= "") . words)
          . map (\(_, x, _) -> if not $ isDigit x then ' ' else x)
      )
      ( filter
          ( ((not . null) . filter (\x -> not (isDigit x) && x /= '.'))
              . (\(x, y, z) -> x <> y <> z)
              . unzip3
          )
          ( splitWhen
              ( \(x, y, z) ->
                  not (isDigit y) && x == '.' && y == '.' && z == '.'
              )
              $ zip3 x y z
          )
      )

chunks :: [String] -> [[String]]
chunks all@(x : y : xs) = [x, y] : chunks' all

chunks' :: [String] -> [[String]]
chunks' (x : y : z : xs) = [x, y, z] : chunks' (y : z : xs)
chunks' [x, y] = [[y, x]]
chunks' [x] = [[x]]
chunks' [] = []

{-
  Part Two:

  One of the gears in the engine is wrong. A gear is any symbol * that is adjacent to exactly two part numbers.
  Its gear ratio is the result of multiplying those two numbers together.

  What is the sum of all the gear ratios in your engine schematic?
-}
sumOfAllGearRatios :: String -> Int
sumOfAllGearRatios x =
  ( sum
      . map product
      . filter (\searchCoordinates -> length searchCoordinates == 2)
  )
    ( map
        ( map snd
            . ( \x ->
                  filter (\(z, w) -> (not . null) (x `intersect` z)) numberInfo
              )
        )
        searchCoordinates
    )
  where
    searchCoordinates = map (\(x, y) -> [(z, w) | z <- [x - 1, x, x + 1], w <- [y - 1, y, y + 1]]) $ asteriskCoordinates x
    numberInfo = numberCoordinatesAndValues x

asteriskCoordinates :: String -> [(Int, Int)]
asteriskCoordinates x = map fst . filter (\((_, _), z) -> z == '*') $ mconcat (zipWith (\x y -> zipWith (\z w -> ((x, z), w)) [0 ..] y) [0 ..] (lines x))

numberCoordinatesAndValues :: String -> [([(Int, Int)], Int)]
numberCoordinatesAndValues x = map ((\(x, y) -> (x, fromMaybe 0 (readMaybe y :: Maybe Int))) . unzip) (filter (not . null) $ mconcat (zipWith (\x y -> splitWhen ((not . isDigit) . snd) $ zipWith (\z w -> ((x, z), w)) [0 ..] y) [0 ..] (lines x)))