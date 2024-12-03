module Main where

import Day2 (inputPath, solve2)

main :: IO ()
main = do
  input <- readFile inputPath
  print $ solve2 input
