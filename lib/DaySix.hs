module DaySix where

import Data.Char

{-
  Part One:

  You manage to sign up as a competitor in the boat races just in time.

  As part of signing up, you get a sheet of paper that lists the time allowed
  for each race and slo the best distance ever recorded in that race. To win,
  you need to make sure you go farther in the race than the current record holder.

  Holding down the button charges the boat and releasing the button allows the boat to move.

  Your toy boat has a starting speed of 0, for each millisecond you spend at the beginning of the race
  holding down the button, the boat's speed increases by one millimeter per millisecond

  To see how much margin of error you have, determine the number of ways you can beat the record.
  What do you get if you multiply these numbers together?
-}
productOfAllWaysToWin :: String -> Int
productOfAllWaysToWin x = product . (\x -> zipWith waysToWin (head x) (last x)) . map (map read . words) . lines $ filter (\x -> isDigit x || x == '\n' || x == ' ') x

waysToWin :: Int -> Int -> Int
waysToWin time distance = length $ [x | x <- [1 .. time], x * (time - x) > distance]

{-
  Part Two:

  The kerning is very bad. It's actually one giant race (ignore the spaces).
  How many ways are there to win the big race?
-}
waysToWinBigRace :: String -> Int
waysToWinBigRace x = (\(x, y) -> abs (x - y)) . uncurry getQuadriticTimes . (\x -> (read $ head x :: Int, read $ last x :: Int)) . lines $ filter (\x -> isDigit x || x == '\n') x
  where
    getQuadriticTimes :: Int -> Int -> (Int, Int)
    getQuadriticTimes time distance = (ceiling $ 0.5 * (fromIntegral time - (sqrt . fromIntegral) (time * time - 4 * (distance + 1))), ceiling $ 0.5 * (fromIntegral time + (sqrt . fromIntegral) (time * time - 4 * (distance + 1))))