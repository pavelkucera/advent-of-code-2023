#!/usr/bin/env stack
-- stack script --resolver lts-21.25 --package megaparsec --package containers

import Data.Char (isSpace)
import Data.List

-- Trim leading and trailing spaces
trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace

-- Parse a line into a tuple of two lists of integers
parseLine :: String -> ([Int], [Int])
parseLine line = (map read . words $ a, map read . words $ b)
  where
    (a, b) = fmap (trim . drop 1) $ break (== '|') line

-- Calculate points for a single card
calculatePoints :: ([Int], [Int]) -> Int
calculatePoints (winning, nums) = foldl' (\acc n -> if n `elem` winning then acc * 2 else acc) 1 matched
  where
    matched = filter (`elem` winning) nums

main :: IO ()
main = do
  contents <- getContents
  let cards = map parseLine $ lines contents
  let totalPoints = sum $ map calculatePoints cards
  print totalPoints
