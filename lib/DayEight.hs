module DayEight where

import Data.List
import Data.List.Split
import qualified Data.Map as M

{-
  Part One:

  While riding the camel, you see a document that contains a list of
  left/right instructions, and the rest of the documents seem to describe
  some kind of network of labeled nodes.

  Repeat the instructions until you reach ZZZ
-}
stepsUntilDestinationReached :: String -> Int
stepsUntilDestinationReached xs = length . takeWhile (/= "ZZZ") $ scanl (getNextNode (M.fromList networkMap)) "AAA" (mconcat $ repeat instructions)
  where
    instructions = head $ lines xs
    networkMap = map ((\x -> (head x, (x !! 1, last x))) . wordsBy (`elem` "=,") . filter (`notElem` " ()")) $ drop 2 $ lines xs

getNextNode :: M.Map String (String, String) -> String -> Char -> String
getNextNode nMap currentNode instruction = maybe "" (\(x, y) -> if instruction == 'L' then x else y) (M.lookup currentNode nMap)

{-
  Part Two:

  Your starting nodes are all nodes that end with A
  Your ending nodes are all nodes that end with Z
-}
stepsUntilAllDestinationsReached :: String -> Int
stepsUntilAllDestinationsReached xs = foldr1 lcm $ map (\x -> (length . takeWhile (not . isSuffixOf "Z")) $ scanl (getNextNode (M.fromList networkMap)) x (mconcat $ repeat instructions)) allStartingNodes
  where
    instructions = head $ lines xs
    networkMap = map ((\x -> (head x, (x !! 1, last x))) . wordsBy (`elem` "=,") . filter (`notElem` " ()")) $ drop 2 $ lines xs
    allStartingNodes = filter ("A" `isSuffixOf`) $ map fst networkMap