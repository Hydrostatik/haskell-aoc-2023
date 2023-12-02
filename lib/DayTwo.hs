module DayTwo where

import Data.List

{-
  Part One:

  You are on Snow Island. The elf shows you a small bag and some cubes which
  are either red, green, or blue. Each time you play this game, he will hide a secret
  number of cubes of each color in the bag, and your goal is to figure out
  information about the number of cubes.

  Each game is listed with its ID number followed by a semicolon-separate list
  of subsets of cubes that were revealed from the bag.

  The Elf would like to know which games would have been possible if the bag
  contained only 12 red cubes, 13 green cubes and 14 blue cubes. What is the sum of the IDs of the games?
-}
sumOfValidGameIDs :: String -> Int
sumOfValidGameIDs x = foldr (\x acc -> gameID x + acc) 0 . filter (all turnMeetsPartOneCriteria . turns) . map readGame $ lines x

{-
  Part Two:

  In each game what is the fewest number of cubes of each color?
  Find the sum of the product of the least amount of cubes required to play each game
-}
productOfMinimumCubesRequired :: String -> Int
productOfMinimumCubesRequired x = sum $ map ((\x -> maximum (map fst x) * maximum (map snd x) * maximum (map thd x)) . turns . readGame) $ lines x
  where
    fst (x, _, _) = x
    snd (_, x, _) = x
    thd (_, _, x) = x

data Game = Game
  { gameID :: Int,
    turns :: [(Int, Int, Int)]
  }
  deriving (Show)

turnMeetsPartOneCriteria :: (Int, Int, Int) -> Bool
turnMeetsPartOneCriteria (redCubes, greenCubes, blueCubes) = redCubes <= 12 && greenCubes <= 13 && blueCubes <= 14

readGame :: String -> Game
readGame x = Game _gameID _turns
  where
    _gameID = read . last . words . head $ splitOn ':' x
    _turns = map readTurn . splitOn ';' . last $ splitOn ':' x

readTurn :: String -> (Int, Int, Int)
readTurn input = foldr ((\(x, y, z) (x1, y1, z1) -> (x + x1, y + y1, z + z1)) . matchColors) (0, 0, 0) (splitOn ',' input)
  where
    matchColors x
      | "red" `isSuffixOf` x = (extractNumber x, 0, 0)
      | "green" `isSuffixOf` x = (0, extractNumber x, 0)
      | "blue" `isSuffixOf` x = (0, 0, extractNumber x)
      | otherwise = (0, 0, 0)
    extractNumber x = read . head $ words x

splitOn :: Char -> String -> [String]
splitOn p s = case dropWhile (== p) s of
  "" -> []
  s' -> w : splitOn p s''
    where
      (w, s'') = break (== p) s'