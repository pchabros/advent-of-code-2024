module Day5 where

import Data.List (elemIndex)

inputPath :: String
inputPath = "./inputs/day-5.txt"

replaceAll :: (Eq a) => a -> a -> [a] -> [a]
replaceAll from to = map (\x -> if x == from then to else x)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a, b) = (f a, f b)

splitOn :: (a -> Bool) -> [a] -> ([a], [a])
splitOn p xs = (before, filter (not . p) after)
  where
    (before, after) = break p xs

getMiddle :: [a] -> a
getMiddle xs = xs !! index
  where
    index = length xs `div` 2

type Rule = (Int, Int)

type Update = [Int]

parseRules :: [String] -> [Rule]
parseRules = map (mapTuple read . splitOn (== '|'))

parseUpdates :: [String] -> [Update]
parseUpdates = map parseUpdate
  where
    parseUpdate = map read . words . replaceAll ',' ' '

parseInput :: String -> ([Rule], [Update])
parseInput input = (parseRules rules, parseUpdates updates)
  where
    (rules, updates) = splitOn (== "") $ lines input

isUpdateRule :: Update -> Rule -> Bool
isUpdateRule update (x1, x2) = (x1 `elem` update) && (x2 `elem` update)

filterRules :: Update -> [Rule] -> [Rule]
filterRules update = filter (isUpdateRule update)

isUpdateValid :: [Rule] -> Update -> Bool
isUpdateValid rules update = all (ruleSatisfied update) updateRules
  where
    ruleSatisfied update' (x1, x2) = x1 `elemIndex` update' < x2 `elemIndex` update'
    updateRules = filterRules update rules

getMiddleOfInvalid :: [Rule] -> Update -> Int
getMiddleOfInvalid rules update = head $ filter (\page -> nPagesBefore page == middleIndex) update
  where
    updateRules = filterRules update rules
    nPagesBefore page = length $ filter (\(_, x) -> page == x) updateRules
    middleIndex = length update `div` 2

solve1 :: String -> Int
solve1 input = sum $ map getMiddle validUpdates
  where
    validUpdates = filter (isUpdateValid rules) updates
    (rules, updates) = parseInput input

solve2 :: String -> Int
solve2 input = sum $ map (getMiddleOfInvalid rules) invalidUpdates
  where
    invalidUpdates = filter (not . isUpdateValid rules) updates
    (rules, updates) = parseInput input
