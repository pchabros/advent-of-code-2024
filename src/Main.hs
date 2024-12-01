module Main where

import Data.List (sort, transpose)
import Day1 (inputPath)

parseIdsPair :: String -> [Integer]
parseIdsPair line = map read $ words line

sortIdLists :: [[Integer]] -> [[Integer]]
sortIdLists pairs = transpose $ map sort $ transpose pairs

calculatePairDistance :: [Integer] -> Integer
calculatePairDistance [] = 0
calculatePairDistance [_] = 0
calculatePairDistance (id1 : id2 : _) = abs (id1 - id2)

main :: IO ()
main = do
  input <- readFile inputPath
  let pairs = map parseIdsPair $ lines input
  print $ sum $ map calculatePairDistance $ sortIdLists pairs
