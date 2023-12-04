module DayFour where

import Data.List
import Data.List.Split

{-
  Part One:

  The Elf has many scratchcards. It looks like each card has two list of numbers separated by
  a vertical bar (|): a list of winning numbers and then a list of numbers you have.

  You have to figure out which of the numbers you have appear in the list of winning numbers.
  The first match makes the card worth one point and each match after the first doubles the point value of
  that card

  How many points are all the scratchcards worth in total?
-}
totalWorthOfScratchCards :: String -> Int
totalWorthOfScratchCards x = sum . map howManyPointsWonOnScratchcard $ lines x

howManyPointsWonOnScratchcard :: String -> Int
howManyPointsWonOnScratchcard x = (\x -> if null x then 0 else 2 ^ (length x - 1)) . uncurry intersect . (\x -> (head x, last x)) . map words . splitOn "|" . last $ splitOn ":" x

{-
  Part Two:

  There's no such thing as "points". Instead, scratchcards only cause you to win more scratchcards equal to the
  number of winning numbers you have.

  They do this until none of the copies cause you to win any more cards. How many total scratchcards do you end up with?
-}
totalNumberOfScratchCards :: String -> Int
totalNumberOfScratchCards x = sum . map (\(x, y, z) -> y) . totalCardsWon $ zip3 [1 ..] (repeat 1) (map howManyScratchCardsWon $ lines x)

totalCardsWon :: [(Int, Int, Int)] -> [(Int, Int, Int)]
totalCardsWon (x : xs) = if numerOfCards > 0 then x : totalCardsWon (map (\(y, z, w) -> if y < cardNumber + cardsWon + 1 then (y, z + numerOfCards, w) else (y, z, w)) xs) else x : totalCardsWon xs
  where
    cardNumber = (\(x, _, _) -> x) x
    cardsWon = (\(_, _, x) -> x) x
    numerOfCards = (\(_, x, _) -> x) x
totalCardsWon [] = []

howManyScratchCardsWon :: String -> Int
howManyScratchCardsWon x = length . uncurry intersect . (\x -> (head x, last x)) . map words . splitOn "|" . last $ splitOn ":" x
