module DayTen where

import qualified Data.Map as M
import Data.Maybe
import Data.Tuple

{-
 Part One:

 You don't have any thermals to glide on, you find pipes that are arranged in a two-dimensional grid of tiles:
 `|` is a vertical pipe connecting north and south
 `-` is a horizontal pipe connecting east and west
 `L` is a 90-bend connecting north and east
 `J` is a 90-bend connecting north and west
 `7` is a 90-bend connecting south and west
 `F` is a 90-bend connecting south and east
 `.` is ground; there is no pipe in this tile.
 `S` is the starting position

 How many steps along the loop does it take to get from the starting position to the point farthest from the starting position?
-}
totalStepsForLoop :: String -> Int
totalStepsForLoop xs = (`div` 2) . length $ completePath xs

{-
  Part Two:

  How many tiles are enclosed by the loop?
-}
enclosedTilesInLoop :: String -> Int
enclosedTilesInLoop xs = (abs (loopArea completePath') - length completePath' + 3) `div` 2
  where
    completePath' = completePath xs
    loopArea (x : y : xs) = (snd x + snd y) * (fst y - fst x) + loopArea (y : xs)
    loopArea [_] = 0

completePath :: String -> [(Int, Int)]
completePath xs = traversePipes grid startingPoint startingPoint
  where
    grid = generateGrid xs
    startingPoint = findStartingPosition grid

generateGrid :: String -> M.Map (Int, Int) Char
generateGrid xs = M.fromList $ concat (zipWith (\x y -> zipWith (\z w -> ((z, x), w)) [0 ..] y) [0 ..] (lines xs))

findStartingPosition :: M.Map (Int, Int) Char -> (Int, Int)
findStartingPosition xs = reverseMap M.! 'S'
  where
    reverseMap = M.fromList $ map swap $ M.toList xs

traversePipes :: M.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
traversePipes mp p p1@(x, y)
  | validConnectionNorth (getChar p1) (getChar (x, y - 1)) && p /= (x, y - 1) = p1 : traversePipes mp p1 (x, y - 1)
  | validConnectionEast (getChar p1) (getChar (x + 1, y)) && p /= (x + 1, y) = p1 : traversePipes mp p1 (x + 1, y)
  | validConnectionSouth (getChar p1) (getChar (x, y + 1)) && p /= (x, y + 1) = p1 : traversePipes mp p1 (x, y + 1)
  | validConnectionWest (getChar p1) (getChar (x - 1, y)) && p /= (x - 1, y) = p1 : traversePipes mp p1 (x - 1, y)
  | otherwise = [p1]
  where
    getChar x = fromMaybe ' ' $ M.lookup x mp

validConnectionNorth :: Char -> Char -> Bool
validConnectionNorth a b = a `elem` "S|LJ" && b `elem` "|F7"

validConnectionSouth :: Char -> Char -> Bool
validConnectionSouth a b = a `elem` "S|F7" && b `elem` "|LJ"

validConnectionEast :: Char -> Char -> Bool
validConnectionEast a b = a `elem` "S-LF" && b `elem` "-J7"

validConnectionWest :: Char -> Char -> Bool
validConnectionWest a b = a `elem` "S-J7" && b `elem` "-LF"