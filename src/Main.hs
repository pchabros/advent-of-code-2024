module Main where

import Data.List (transpose)
import Day1 (inputPath)

parseColumns :: String -> ([Integer], [Integer])
parseColumns input = (columns !! 0, columns !! 1)
  where
    columns = transpose $ map (map read . words) $ lines input

count :: Integer -> [Integer] -> Integer
count x = toInteger . length . filter (== x)

calculateScore :: ([Integer], [Integer]) -> Integer
calculateScore (col1, col2) = foldl (\acc number -> acc + number * count number col2) 0 col1

main :: IO ()
main = do
  input <- readFile inputPath
  print $ calculateScore $ parseColumns input
