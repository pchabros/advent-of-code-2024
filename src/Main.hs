module Main where

inputPath :: String
inputPath = "./inputs/day-2.txt"

parseReport :: String -> [Integer]
parseReport = map read . words

rolling :: (Integer -> Integer -> Integer) -> [Integer] -> [Integer]
rolling f xs = zipWith f xs (tail xs)

rollingDiff :: [Integer] -> [Integer]
rollingDiff = rolling (-)

between :: Integer -> Integer -> Integer -> Bool
between lower upper x = x >= lower && x <= upper -- TODO: try to write without `x`

stepsSafe :: [Integer] -> Bool
stepsSafe steps = all (between 1 3) steps || all (between (-3) (-1)) steps

isReportValid :: [Integer] -> Bool
isReportValid report = stepsSafe $ rollingDiff report

main :: IO ()
main = do
  input <- readFile inputPath
  print $ length $ filter isReportValid $ map parseReport (lines input)
