module Main where

inputPath :: String
inputPath = "./inputs/day-5-test.txt"

solve1 :: String -> Int
solve1 _ = 1

main :: IO ()
main = do
  input <- readFile inputPath
  print $ solve1 input
