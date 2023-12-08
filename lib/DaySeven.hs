module DaySeven where

import Data.List
import Data.Ord

{-
  Part One:

  It's our job to figure out why the parts stopped and how to fix it.

  You're offered to play Camel Cards. In Camel Cards you get a list of hands,
  your goal is to order them based on the strength of each hand.

  A hand consists of five cards labeled
  A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, 2.

  A is the strongest and 2 is the weakest

  Every hand has exactly one type. From strongest to weakest:
  Five of a kind: where all five cards have the same label
  Four of a kind: where four cards have the same label and one card has a different label
  Full house: where three cards have the same label, and the 2 cards have the same label
  Three of a kind: where three cards have the same label, and the remaining two cards are each different
  Two pair: two cards share one label, two other cards share another label
  One pair: two cards share one label, and the other three cards have a different label
  High card: where all card labels are distinct

  Every full house is stronger than any three of a kind

  If two hands have the same type, a second ordering rule takes effect. Start by comparing the first card in each hand.
  If these cards are different, the hand with the stronger first card is considered stronger. If the first card in each hand have the same label,
  however, then move on to considering the second card in each hand. If they differ, the hand with the higher second card wins;
  otherwise, continue with the third card in each hand, then the fourth, then the fifth.

  So, 33332 and 2AAAA are both four of a kind hands, but 33332 is stronger because its first card is stronger.
  Similarly, 77888 and 77788 are both a full house, but 77888 is stronger because its third card is stronger
  (and both hands have the same first and second card).

  Put your hands in order of strength (weakest to strongest) then multiply by the bid amount and all them all up together.
  What are the toal winnings?
-}
totalWinnings :: String -> Int
totalWinnings xs = sum . zipWith (\x y -> x * snd y) [1 ..] $ sortBy (comparing fst) . map ((\x -> (parseHand parseCard $ head x, read $ last x :: Int)) . words) $ lines xs

data Card
  = Joker
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | T
  | J
  | Q
  | K
  | A
  deriving (Show, Eq, Ord)

parseCard :: Char -> Card
parseCard '2' = Two
parseCard '3' = Three
parseCard '4' = Four
parseCard '5' = Five
parseCard '6' = Six
parseCard '7' = Seven
parseCard '8' = Eight
parseCard '9' = Nine
parseCard 'T' = T
parseCard 'J' = J
parseCard 'Q' = Q
parseCard 'K' = K
parseCard 'A' = A

data Hand
  = HighCard [Card]
  | OnePair [Card]
  | TwoPair [Card]
  | ThreeOfAKind [Card]
  | FullHouse [Card]
  | FourOfAKind [Card]
  | FiveOfAKind [Card]
  deriving (Show, Eq, Ord)

parseHand :: (Char -> Card) -> String -> Hand
parseHand f xs
  | any (\x -> length x == 5) (group $ sort cards) = FiveOfAKind cards
  | any (\x -> length x == 4) (group $ sort cards) = FourOfAKind cards
  | (\x -> any (\x -> length x == 3) x && any (\x -> length x == 2) x) (group $ sort cards) = FullHouse cards
  | any (\x -> length x == 3) (group $ sort cards) = ThreeOfAKind cards
  | (\x -> length x == 2) $ filter (\x -> length x == 2) (group $ sort cards) = TwoPair cards
  | any (\x -> length x == 2) (group $ sort cards) = OnePair cards
  | otherwise = HighCard cards
  where
    cards = map f xs

{-
  Part Two:

  The J card now acts like a wildcard, but is also the weakest card on its own.

  What are the new total winnings?
-}
totalWinnings' :: String -> Int
totalWinnings' xs = sum . zipWith (\x y -> x * snd y) [1 ..] $ sortBy (\x y -> compare (fst x) (fst y)) . map ((\x -> (considerJokers . parseHand parseCard' $ head x, read $ last x :: Int)) . words) $ lines xs

considerJokers :: Hand -> Hand
considerJokers (FiveOfAKind x) = FiveOfAKind x
considerJokers (FourOfAKind x) = if numberOfJokers x `elem` [1, 4] then FiveOfAKind x else FourOfAKind x
considerJokers (FullHouse x) = if numberOfJokers x `elem` [2, 3] then FiveOfAKind x else FullHouse x
considerJokers (ThreeOfAKind x) = if numberOfJokers x `elem` [1, 3] then FourOfAKind x else ThreeOfAKind x
considerJokers (TwoPair x)
  | numberOfJokers x == 1 = FullHouse x
  | numberOfJokers x == 2 = FourOfAKind x
  | otherwise = TwoPair x
considerJokers (OnePair x) = if numberOfJokers x `elem` [1, 2] then ThreeOfAKind x else OnePair x
considerJokers (HighCard x) = if numberOfJokers x == 1 then OnePair x else HighCard x

numberOfJokers :: [Card] -> Int
numberOfJokers x = length $ filter (== Joker) x

parseCard' :: Char -> Card
parseCard' '2' = Two
parseCard' '3' = Three
parseCard' '4' = Four
parseCard' '5' = Five
parseCard' '6' = Six
parseCard' '7' = Seven
parseCard' '8' = Eight
parseCard' '9' = Nine
parseCard' 'T' = T
parseCard' 'J' = Joker
parseCard' 'Q' = Q
parseCard' 'K' = K
parseCard' 'A' = A