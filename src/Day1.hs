module Day1 where

import Data.List (sort, transpose)

inputPath :: String
inputPath = "./inputs/day-1.txt"

-- Puzzle 1

parseIdsPair :: String -> [Integer]
parseIdsPair line = map read $ words line

sortIdLists :: [[Integer]] -> [[Integer]]
sortIdLists pairs = transpose $ map sort $ transpose pairs

calculatePairDistance :: [Integer] -> Integer
calculatePairDistance [] = 0
calculatePairDistance [_] = 0
calculatePairDistance (id1 : id2 : _) = abs (id1 - id2)

solve1 :: String -> Integer
solve1 input = sum $ map calculatePairDistance $ sortIdLists pairs
  where
    pairs = map parseIdsPair $ lines input

-- Puzzle 2

parseColumns :: String -> ([Integer], [Integer])
parseColumns input = (columns !! 0, columns !! 1)
  where
    columns = transpose $ map (map read . words) $ lines input

count :: Integer -> [Integer] -> Integer
count x = toInteger . length . filter (== x)

calculateScore :: ([Integer], [Integer]) -> Integer
calculateScore (col1, col2) = foldl (\acc number -> acc + number * count number col2) 0 col1

solve2 :: String -> Integer
solve2 input = calculateScore $ parseColumns input
