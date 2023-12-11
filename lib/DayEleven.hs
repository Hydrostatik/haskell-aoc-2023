module DayEleven where

import Data.Bifunctor
import Data.List

{-
  Part One:

  A researcher promises to take you to the hotsprings once he's done with this research.

  He's collected a bunch of data and compiled the data into a single giant image.
  The image includes empty space (.) and galaxies (#)

  The research is trying to figure out the sum of the lengths of the shortest
  path between every pair of galaxies. However, there's a catch: the universe
  expanded in the time it too the light from those galaxies to reach the observatory.

  Due to something involving gravitational effects, only some space expands.
  Any row or columns that contain no galaxies should be twice as big.

  Expand the universe and then find the length of the shortest path between every pair
  of galaxies. What is the sum of these lengths?
-}
sumOfShortestPathsBetweenGalaxies :: String -> Int
sumOfShortestPathsBetweenGalaxies = (`div` 2) . sum . findShortestPaths . generateGalaxyCoordinates . expandUniverse . lines

expandUniverse :: [String] -> String
expandUniverse = unlines . transpose . expandInOneDirection . transpose . expandInOneDirection
  where
    expandInOneDirection :: [String] -> [String]
    expandInOneDirection = foldr (\x acc -> if all (== '.') x then x : x : acc else x : acc) []

generateGalaxyCoordinates :: String -> [(Int, Int)]
generateGalaxyCoordinates xs = map fst . filter (\x -> snd x == '#') $ concat (zipWith (\x y -> zipWith (\z w -> ((z, x), w)) [0 ..] y) [0 ..] (lines xs))

findShortestPaths :: [(Int, Int)] -> [Int]
findShortestPaths xs = [abs (fst x - fst y) + abs (snd x - snd y) | x <- xs, y <- xs, x /= y]

{-
  Part Two:

  Replace each empty row and column by 1,000,000 empty rows
  What is the sum of these lenghts?
-}
sumOfShortestPathsBetweenGalaxies' :: String -> Int
sumOfShortestPathsBetweenGalaxies' = (`div` 2) . sum . findShortestPaths . (\x -> mapExpansionOfGalaxies (fst x) 1000000 (snd x)) . (\x -> (expandedUniverseRowsAndColumns $ lines x, generateGalaxyCoordinates x))

expandedUniverseRowsAndColumns :: [String] -> ([Int], [Int])
expandedUniverseRowsAndColumns = \x -> (expandInOneDirection x, expandInOneDirection $ transpose x)
  where
    expandInOneDirection :: [String] -> [Int]
    expandInOneDirection = map fst . filter (all (== '.') . snd) . zip [0 ..]

mapExpansionOfGalaxies :: ([Int], [Int]) -> Int -> [(Int, Int)] -> [(Int, Int)]
mapExpansionOfGalaxies (r, c) ev = map (\(x, y) -> (expandPoint x c, expandPoint y r))
  where
    expandPoint :: Int -> [Int] -> Int
    expandPoint x (y : ys)
      | x > y = (ev - 1) + expandPoint x ys
      | otherwise = x
    expandPoint x [] = x

universe = "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#....."