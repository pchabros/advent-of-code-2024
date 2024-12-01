module Day1 where

import Data.List (sort, transpose)

inputPath :: String
inputPath = "./inputs/day-1.txt"

sortIdLists :: [[Integer]] -> [[Integer]]
sortIdLists pairs = transpose $ map sort $ transpose pairs

calculatePairDistance :: [Integer] -> Integer
calculatePairDistance [] = 0
calculatePairDistance [_] = 0
calculatePairDistance (id1 : id2 : _) = abs (id1 - id2)

solution1 :: String
solution1 = "foo"
