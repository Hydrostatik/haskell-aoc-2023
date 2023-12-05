module DayFive where

import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

{-
  Part One:

  We need to help the island with their food production problem.

  The almanac lists all seeds that need to be planted.
  It also lists what type of soil they use with each kind of seed,
  what type of fertilizer to use with each kind of soil,
  what type of water to use with each kind of fertilizer

  Every type of seed, soil, fertilizer and so on are identified by a number
  but that number is reused.

  Any source numbers that aren't mapped correspond to the same destination number.
  So, seed number 10 corresponds to soil number 10.

  What is the lowest location number that corresponds to any of the initial seed numbers?
-}
lowestLocationNumber :: String -> Int
lowestLocationNumber x = minimum $ mapFromSeedToLocation x

mapFromSeedToLocation :: String -> [Int]
mapFromSeedToLocation x = foldl (flip getMappings) seeds ranges
  where
    input = parseInput x
    seeds = map (\x -> read x :: Int) . words $ head input
    ranges = map getRangeForMap $ tail input

getMappings :: [(Int, Int, Int)] -> [Int] -> [Int]
getMappings ys = fmap (getMapping ys)

getMapping :: [(Int, Int, Int)] -> Int -> Int
getMapping xs y = maybe y (\(destinationRange, sourceRange, rangeLength) -> destinationRange + (y - sourceRange)) $ find (\(destinationRange, sourceRange, rangeLength) -> y >= sourceRange && y <= sourceRange + rangeLength) xs

{-
  Part Two:

  What is the lowest location number that corresponds to any initial seed numbers if the
  input describes a range of seed numbers that come in pairs?
-}
lowestLocationNumber' :: String -> Int
lowestLocationNumber' x = minimum . map fst $ mapFromSeedToLocation' x

mapFromSeedToLocation' :: String -> [(Int, Int)]
mapFromSeedToLocation' x = foldl (flip getMappings') seeds ranges
  where
    input = parseInput x
    seeds = seedRange . map (\x -> read x :: Int) . words $ head input
    ranges = map getRangeForMap $ tail input

seedRange :: [Int] -> [(Int, Int)]
seedRange (x : y : xs) = (x, y) : seedRange xs
seedRange [x] = []
seedRange [] = []

getMappings' :: [(Int, Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
getMappings' ys = foldr ((<>) . getMapping' ys) []

getMapping' :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
getMapping' xs (x, y) =
  maybe
    [(x, y)]
    (\z -> findRange z (x, y))
    (find (\(destinationRange, sourceRange, rangeLength) -> x + y >= sourceRange && x <= sourceRange + rangeLength) xs)
  where
    findRange (destinationRange, sourceRange, rangeLength) (x, y)
      | x < sourceRange && x + y <= sourceRange + rangeLength = [(destinationRange, rangeLength - (x + y - sourceRange))]
      | x > sourceRange && x + y <= sourceRange + rangeLength = [(destinationRange + (x - sourceRange), rangeLength - (x + y - sourceRange))]
      | x > sourceRange && x <= sourceRange + rangeLength = (destinationRange + (x - sourceRange), rangeLength - (x - sourceRange)) : getMapping' xs (sourceRange + rangeLength + 1, x + y - (sourceRange + rangeLength))
      | otherwise = [(x, y)]

parseInput :: String -> [String]
parseInput x = splitOn "\n\n \n" $ filter (\x -> isDigit x || x == '\n' || x == ' ') x

getRangeForMap :: String -> [(Int, Int, Int)]
getRangeForMap x = map getRanges (lines x)

getRanges :: String -> (Int, Int, Int)
getRanges x = (\x -> (readInt $ head x, readInt $ x !! 1, readInt $ x !! 2)) $ words x
  where
    readInt x = read x :: Int

debugInput :: String
debugInput = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4"