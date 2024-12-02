module Main where

inputPath :: String
inputPath = "./inputs/day-2.txt"

between :: Integer -> Integer -> Integer -> Bool
between lower upper x = x >= lower && x <= upper -- TODO: try to write without `x`

parseReport :: String -> [Integer]
parseReport = map read . words

deleteNth :: Integer -> [Integer] -> [Integer]
deleteNth _ [] = []
deleteNth n (x : xs)
  | n == 0 = xs
  | otherwise = x : deleteNth (n - 1) xs

getSubsets :: [Integer] -> [[Integer]]
getSubsets set = map (`deleteNth` set) [0 .. (toInteger $ length set - 1)]

rolling :: (Integer -> Integer -> Integer) -> [Integer] -> [Integer]
rolling f xs = zipWith f xs (tail xs)

rollingDiff :: [Integer] -> [Integer]
rollingDiff = rolling (-)

isReportValid :: [Integer] -> Bool
isReportValid report = isValid report || isValid (reverse report)
  where
    isValid r = all (between 1 3) $ rollingDiff r

isDampenerValid :: [Integer] -> Bool
isDampenerValid report = any isReportValid $ getSubsets report

solve2 :: String -> Int
solve2 input = length $ filter isDampenerValid $ map parseReport (lines input)

main :: IO ()
main = do
  input <- readFile inputPath
  print $ solve2 input
