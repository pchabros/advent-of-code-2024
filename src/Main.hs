module Main where

inputPath :: String
inputPath = "./inputs/day-5.txt"

main :: IO ()
main = do
  input <- readFile inputPath
  print input
