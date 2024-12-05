module Day4 where

-- import Data.List (isPrefixOf, transpose)

inputPath :: String
inputPath = "./inputs/day-4.txt"

-- enumerate :: [a] -> [(Int, a)]
-- enumerate = zip [0 ..]

-- encode :: String -> [Int]
-- encode = map encode'
--   where
--     encode' 'X' = 1
--     encode' 'M' = 2
--     encode' 'A' = 3
--     encode' 'S' = 4
--     encode' _ = 0

-- xmasCode :: [Int]
-- xmasCode = [1 .. 4]

-- diagonals :: [[Int]] -> [[Int]]
-- diagonals matrix = transpose $ map (\(i, row) -> replicate i 0 ++ row) (enumerate matrix)

-- count :: [Int] -> Int
-- count [] = 0
-- count xs = (if xmasCode `isPrefixOf` xs || reverse xmasCode `isPrefixOf` xs then 1 else 0) + count (tail xs)

-- solve1 :: String -> Int
-- solve1 input = sumLines matrix + sumLines (transpose matrix) + sumDiagonals matrix + sumDiagonals (map reverse matrix)
--   where
--     sumLines m = sum $ map count m
--     sumDiagonals m = sumLines $ diagonals m
--     matrix = map encode $ lines input

-- ------------------------------------------------------------------------------------------------------------------

countXMas' :: Int -> Int -> [Char] -> Int
countXMas' idx width xs
  | idx + 2 * width  > length xs = 0
  | foundXMas1 xs || foundXMas2 xs || foundXMas3 xs || foundXMas4 xs = 1 + countXMas' (idx + 1) width xs
  | otherwise = countXMas' (idx + 1) width xs
  where
    foundXMas1 xs' = not inNextLine && (xs' !! idx == 'M') && (xs' !! (idx + 2) == 'M') && (xs' !! (idx + width + 1) == 'A') && (xs' !! (idx + 2 * width) == 'S') && (xs' !! (idx + 2 * width + 2) == 'S') -- TOP (9)
    foundXMas2 xs' = not inNextLine && (xs' !! idx == 'S') && (xs' !! (idx + 2) == 'S') && (xs' !! (idx + width + 1) == 'A') && (xs' !! (idx + 2 * width) == 'M') && (xs' !! (idx + 2 * width + 2) == 'M') -- BOT (6)
    foundXMas3 xs' = not inNextLine && (xs' !! idx == 'M') && (xs' !! (idx + 2) == 'S') && (xs' !! (idx + width + 1) == 'A') && (xs' !! (idx + 2 * width) == 'M') && (xs' !! (idx + 2 * width + 2) == 'S') -- LEFT (9)
    foundXMas4 xs' = not inNextLine && (xs' !! idx == 'S') && (xs' !! (idx + 2) == 'M') && (xs' !! (idx + width + 1) == 'A') && (xs' !! (idx + 2 * width) == 'S') && (xs' !! (idx + 2 * width + 2) == 'M') -- RIGHT (6)
    inNextLine = modulo == 0 || modulo == width - 1
    modulo = (idx + 1) `mod` width

countXMas :: [[Char]] -> Int
countXMas matrix = countXMas' 0 width xs
  where
    width = length $ head matrix
    xs = concat matrix

solve2 :: String -> Int
solve2 input = countXMas matrix -- + countXMas (reverse matrix) + countXMas (transpose matrix) + countXMas (transpose $ reverse matrix)
  where
    matrix = lines input
