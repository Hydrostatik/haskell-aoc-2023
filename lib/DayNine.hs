module DayNine where

{-
  Part One:

  You take out your handy Oasis and Sand Instability Sensor and analyze your surroundings.

  The OASIS produces a report of many values and how they are changing over time. Each line in
  the report contains the history of a single value

  To best protect the oasis, your environmental report should include a prediction of the next value
  in each history. To do this, start by making a new sequence from the difference at each step of your history.
  If that sequence is not all zeroes, repeat this process, using the sequence you just generated as the input sequence.
  Once all of your values in your latest sequence are zeroes, you can extrapolate what the next value of the
  original history should be

  What is the sum of these extrapolated values?
-}
predictExtrapolatedValues :: String -> Int
predictExtrapolatedValues xs = sum . map (predictNextValue . map (\x -> read x :: Int) <$> words) $ lines xs

predictNextValue :: [Int] -> Int
predictNextValue xs = (\x -> x + last xs) . sum . map last $ historyOfSequences xs

{-
  Part Two:

  What is the sum of backwards extrapolated values?
-}
predictExtrapolatedValues' :: String -> Int
predictExtrapolatedValues' xs = sum . map (predictLastValue . map (\x -> read x :: Int) <$> words) $ lines xs

predictLastValue :: [Int] -> Int
predictLastValue xs = (\x -> head xs - x) . foldr ((-) . head) 0 $ historyOfSequences xs

historyOfSequences :: [Int] -> [[Int]]
historyOfSequences xs = if all (== 0) nextStep then [] else nextStep : historyOfSequences nextStep
  where
    nextStep = differenceAtEachStep xs

differenceAtEachStep :: [Int] -> [Int]
differenceAtEachStep (x : y : xs) = (y - x) : differenceAtEachStep (y : xs)
differenceAtEachStep [y] = []