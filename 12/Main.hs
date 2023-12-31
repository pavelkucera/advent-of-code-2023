module Main where

import Data.Char (isDigit, isSpace)
import Data.List (group)

data Spring
  = Operational
  | Damaged
  | Unknown
  deriving (Eq, Show)

readRecords :: String -> [([Spring], [Int])]
readRecords = map readRecordLine . lines

readRecord :: String -> [Spring]
readRecord = map spring
  where
    spring '?' = Unknown
    spring '.' = Operational
    spring '#' = Damaged

readRecordLine :: String -> ([Spring], [Int])
readRecordLine s =
  let (record, counts) = break (== ' ') s
   in (readRecord record, map (read . trim) $ splitOn counts ',')

splitOn :: String -> Char -> [String]
splitOn [] _ = []
splitOn text delimiter
  | null text = []
  | otherwise =
      first : splitOn (drop 1 rest) delimiter
  where
    (first, rest) = break (== delimiter) text

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Stupidly generates all the possible replacements of unknown states. A
-- smarter approach would consider current state and the rules. It e.g
-- does not make sense to keep generating a possibility if we have already
-- generated too many broken states.
possibilities :: [Spring] -> [[Spring]]
possibilities =
  foldr combine [[]]
  where
    combine Unknown ss = map (Operational :) ss ++ map (Damaged :) ss
    combine s ss = map (s :) ss

fitsCounts :: [Int] -> [Spring] -> Bool
fitsCounts brokenCounts records =
  map length (filter (all (== Damaged)) $ group records) == brokenCounts

arrangementCount :: [Spring] -> [Int] -> Int
arrangementCount record counts = length . filter (fitsCounts counts) $ possibilities record

main :: IO ()
main = do
  stdin <- getContents
  let records = readRecords stdin
      arrangementCounts = sum . map (uncurry arrangementCount) $ records
  print arrangementCounts
