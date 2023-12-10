module Main where

import DayEight
import DayFive (lowestLocationNumber, lowestLocationNumber')
import DayFour (totalNumberOfScratchCards, totalWorthOfScratchCards)
import DayNine (predictExtrapolatedValues, predictExtrapolatedValues')
import DayOne
import DaySeven (totalWinnings, totalWinnings')
import DaySix (productOfAllWaysToWin, waysToWinBigRace)
import DayTen (enclosedTilesInLoop, totalStepsForLoop)
import DayThree
import DayTwo
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Day One" $ do
    it "Calculate the sum of the parsed calibration values" $ do
      calculateSumOfAllCalibrationValues "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet" `shouldBe` 142
    it "Calculate the sum of the parsed calibration values with stringified digits" $ do
      calculateSumOfAllCalibrationValues' "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen" `shouldBe` 281
  describe "Day Two" $ do
    it "Find the sum of all valid game ids" $ do
      sumOfValidGameIDs "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n" `shouldBe` 8
    it "Find the sum of the product of the minimum number of cubes required to play every game" $ do
      productOfMinimumCubesRequired "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n" `shouldBe` 2286
  describe "Day Three" $ do
    it "Get the sum of all the engine part numbers" $ do
      findSumOfAllPartNumbers "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.." `shouldBe` 4361
    it "Gets the sum of all gear ratios" $ do
      sumOfAllGearRatios "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.." `shouldBe` 467835
  describe "Day Four" $ do
    it "Gets total worth of scratchcards" $ do
      totalWorthOfScratchCards "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" `shouldBe` 13
    it "Gets total number of scratchcards won" $ do
      totalNumberOfScratchCards "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" `shouldBe` 30
  describe "Day Five" $ do
    it "Gets the lowest location number for individual seeds" $ do
      lowestLocationNumber "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4" `shouldBe` 35
    it "Gets the lowest location number for seed ranges" $ do
      lowestLocationNumber' "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4" `shouldBe` 46
  describe "Day Six" $ do
    it "Gets the product of all possible ways to win" $ do
      productOfAllWaysToWin "Time:      7  15   30\nDistance:  9  40  200" `shouldBe` 288
    it "Get the possible ways to win for the big race" $ do
      waysToWinBigRace "Time:      7  15   30\nDistance:  9  40  200" `shouldBe` 71503
  describe "Day Seven" $ do
    it "Gets the total winnings" $ do
      totalWinnings "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483" `shouldBe` 6440
    it "Gets the total winnings with Jokers" $ do
      totalWinnings' "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483" `shouldBe` 5905
  describe "Day Eight" $ do
    it "Gets the total steps to reach destination usecase 1" $ do
      stepsUntilDestinationReached "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)" `shouldBe` 6
    it "Gets the total steps to reach destination usecase 2" $ do
      stepsUntilDestinationReached "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)" `shouldBe` 2
    it "Gets the total steps to reach all destinations" $ do
      stepsUntilAllDestinationsReached "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)" `shouldBe` 6
  describe "Day Nine" $ do
    it "Gets the sum of the future prediction of sequences" $ do
      predictExtrapolatedValues "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45" `shouldBe` 114
    it "Gets the sum of the past prediction of sequences" $ do
      predictExtrapolatedValues' "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45" `shouldBe` 2
  describe "Day Ten" $ do
    it "Gets the farthest step in the loop case 1" $ do
      totalStepsForLoop "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF" `shouldBe` 4
    it "Gets the farthest step in the loop case 2" $ do
      totalStepsForLoop "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ" `shouldBe` 8
    it "Gets the enclosed tiles inside the loop case 1" $ do
      enclosedTilesInLoop "..........\n.S------7.\n.|F----7|.\n.||OOOO||.\n.||OOOO||.\n.|L-7F-J|.\n.|II||II|.\n.L--JL--J.\n.........." `shouldBe` 4
    it "Gets the enclosed tiles inside the loop case 2" $ do
      enclosedTilesInLoop "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJIF7FJ-\nL---JF-JLJIIIIFJLJJ7\n|F|F-JF---7IIIL7L|7|\n|FFJF7L7F-JF7IIL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L" `shouldBe` 10
