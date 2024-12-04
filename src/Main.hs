module Main where

import Data.List (isPrefixOf, transpose)

inputPath :: String
inputPath = "./inputs/day-4.txt"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

encode :: String -> [Int]
encode = map encode'
  where
    encode' 'X' = 1
    encode' 'M' = 2
    encode' 'A' = 3
    encode' 'S' = 4
    encode' _ = 0

xmasCode :: [Int]
xmasCode = [1 .. 4]

diagonals :: [[Int]] -> [[Int]]
diagonals matrix = transpose $ map (\(i, row) -> replicate i 0 ++ row) (enumerate matrix)

count :: [Int] -> Int
count [] = 0
count xs = (if xmasCode `isPrefixOf` xs || reverse xmasCode `isPrefixOf` xs then 1 else 0) + count (tail xs)

solve1 :: String -> Int
solve1 input = sumLines matrix + sumLines (transpose matrix) + sumDiagonals matrix + sumDiagonals (map reverse matrix)
  where
    sumLines m = sum $ map count m
    sumDiagonals m = sumLines $ diagonals m
    matrix = map encode $ lines input

-- ------------------------------------------------------------------------------------------------------------------

countXMas' :: Int -> Int -> [Int] -> Int
countXMas' idx width xs
  | length xs < (2 * width + 2) = 0
  | foundXMas xs = 1 + countXMas' (idx + 1) width (tail xs)
  | otherwise = countXMas' (idx + 1) width (tail xs)
  where
    foundXMas xs' = notInNextLine && (head xs' == 2) && (xs' !! 2 == 2) && (xs' !! (width + 1) == 3) && (xs' !! (2 * width) == 4) && (xs' !! (2 * width + 2) == 4)
    notInNextLine = idx `mod` width <= width - 2

countXMas :: [[Int]] -> Int
countXMas matrix = countXMas' 1 width xs
  where
    width = length $ head matrix
    xs = concat matrix

solve2 :: String -> Int
solve2 input = countXMas matrix + countXMas (reverse matrix) + countXMas (transpose matrix) + countXMas (transpose $ reverse matrix)
  where
    matrix = map encode $ lines input

main :: IO ()
main = do
  input <- readFile inputPath
  print $ solve2 input
