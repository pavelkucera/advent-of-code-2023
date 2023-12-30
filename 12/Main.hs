module Main where

import Data.List (group)

data Spring
  = Operational
  | Damaged
  | Unknown
  deriving (Eq, Show)

readRecord :: String -> [Spring]
readRecord = map spring
  where
    spring '?' = Unknown
    spring '.' = Operational
    spring '#' = Damaged

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

fitsRecord :: [Int] -> [Spring] -> Bool
fitsRecord brokenCounts records =
  map length (filter (all (== Damaged)) $ group records) == brokenCounts

main :: IO ()
main = do
  putStrLn "day 12"
