{-# LANGUAGE TupleSections #-}

module Main where

import Data.List (elemIndices, nub)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x : xs) = map (x,) xs ++ pairs xs

inputPath :: String
inputPath = "./inputs/day-8.txt"

type Frequency = Char

type Map = [[Char]]

type Location = (Int, Int)

parseMap :: String -> [[Char]]
parseMap = lines

getFrequencies :: Map -> [Frequency]
getFrequencies = nub . concatMap (filter (/= '.'))

getLocations :: Map -> Frequency -> [Location]
getLocations frequencyMap frequency = concatMap getRowLocations enumeratedRow
  where
    getRowLocations (row, i) = zip [i] (elemIndices frequency row)
    enumeratedRow = zip frequencyMap [0 :: Int ..]

getAntinodes :: Map -> (Location, Location) -> [Location]
getAntinodes frequencyMap ((y1, x1), (y2, x2)) = zip top (if dx < 0 then right else left) ++ zip bot (if dx < 0 then left else right)
  where
    top = [minY, minY - abs dy .. 0]
    left = [minX, minX - abs dx .. 0]
    bot = [maxY, maxY + abs dy .. boundaryY]
    right = [maxX, maxX + abs dx .. boundaryX]
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2
    dx = x2 - x1
    dy = y2 - y1
    boundaryX = length (head frequencyMap) - 1
    boundaryY = length frequencyMap - 1

getFrequencyAninodes :: Map -> Frequency -> [Location]
getFrequencyAninodes frequencyMap frequency = concatMap (getAntinodes frequencyMap) $ pairs (getLocations frequencyMap frequency)

solve2 :: String -> Int
solve2 input = length $ nub $ concatMap (getFrequencyAninodes frequencyMap) frequencies
  where
    frequencies = getFrequencies frequencyMap
    frequencyMap = parseMap input

main :: IO ()
main = do
  input <- readFile inputPath
  print $ solve2 input
