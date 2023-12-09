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
stepsUntilDestinationReached xs = length . takeWhile (/= "ZZZ") $ scanl (getNextNode (M.fromList $ getNetworkMap xs)) "AAA" (cycle $ getInstructions xs)

{-
  Part Two:

  Your starting nodes are all nodes that end with A
  Your ending nodes are all nodes that end with Z
-}
stepsUntilAllDestinationsReached :: String -> Int
stepsUntilAllDestinationsReached xs = foldr1 lcm $ map (\x -> (length . takeWhile (not . isSuffixOf "Z")) $ scanl (getNextNode (M.fromList $ getNetworkMap xs)) x (cycle $ getInstructions xs)) (allStartingNodes xs)

getNextNode :: M.Map String (String, String) -> String -> Char -> String
getNextNode nMap currentNode instruction = (\(x, y) -> if instruction == 'L' then x else y) (nMap M.! currentNode)

getInstructions :: String -> String
getInstructions xs = head $ lines xs

getNetworkMap :: String -> [([Char], ([Char], [Char]))]
getNetworkMap xs = map ((\x -> (head x, (x !! 1, last x))) . wordsBy (`elem` "=,") . filter (`notElem` " ()")) $ drop 2 $ lines xs

allStartingNodes :: String -> [[Char]]
allStartingNodes xs = filter ("A" `isSuffixOf`) . map fst $ getNetworkMap xs
