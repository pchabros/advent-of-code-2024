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

getAllAntinodes :: (Location, Location) -> [Location]
getAllAntinodes ((y1, x1), (y2, x2)) = [(y1 - dy, x1 - dx), (y2 + dy, x2 + dx)]
  where
    dx = x2 - x1
    dy = y2 - y1

isWithinMap :: Map -> Location -> Bool
isWithinMap frequencyMap (y, x) = yWithinMap && xWithinMap
  where
    yWithinMap = y >= 0 && y < length frequencyMap
    xWithinMap = x >= 0 && x < length (head frequencyMap)

getFrequencyAninodes :: Map -> Frequency -> [Location]
getFrequencyAninodes frequencyMap frequency = filter (isWithinMap frequencyMap) antinodesLocations
  where
    antinodesLocations = concatMap getAllAntinodes $ pairs (getLocations frequencyMap frequency)

solve1 :: String -> Int
solve1 input = length $ nub $ concatMap (getFrequencyAninodes frequencyMap) frequencies
  where
    frequencies = getFrequencies frequencyMap
    frequencyMap = parseMap input

main :: IO ()
main = do
  input <- readFile inputPath
  print $ solve1 input
