module Main where

import Text.Regex.TDFA

inputPath :: String
inputPath = "./inputs/day-3.txt"

findMul :: String -> [String]
findMul str = getAllTextMatches (str =~ "mul\\([0-9]{1,3},[0-9]{1,3}\\)")

findMultipliers :: String -> [String]
findMultipliers str = getAllTextMatches (str =~ "[0-9]{1,3}")

multiply :: [String] -> Integer
multiply multipliers = product parsed
  where
    parsed = map read multipliers

findDisabled :: String -> (String, String, String)
findDisabled input = input =~ "don't\\(\\)(.|\n)*?do\\(\\)"

removeDisabled :: String -> String
removeDisabled input = if after' == "" then before' else before' ++ removeDisabled after'
  where
    (before', _, after') = findDisabled input


solve1 :: String -> Integer
solve1 input = sum $ map (multiply . findMultipliers) $ findMul input

solve2 :: String -> Integer
solve2 input = sum $ map (multiply . findMultipliers) $ findMul $ removeDisabled input

main :: IO ()
main = do
  input <- readFile inputPath
  print $ solve2 input
