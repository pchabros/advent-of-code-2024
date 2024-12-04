module Main where

inputPath :: String
inputPath = "./inputs/day-3.txt"

solve1 :: String -> Integer
solve1 input = 1

main :: IO ()
main = do
  input <- readFile inputPath
  print $ solve1 input
