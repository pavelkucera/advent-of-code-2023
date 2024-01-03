{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad
import Control.Monad.State
import Data.Bifunctor
import Data.Char (isDigit, isSpace)
import Data.Foldable
import Data.List (group, intercalate, intersperse)
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Debug.Trace

-- Admission; as I don't do a whole lot of dynamic programming on daily basis,
-- I had to search for a solution, and then I realised that the search space is
-- too big and "just pruning" is not enough. Thanks to that I figured out the
-- whole memoization / dynamic programming part.

data Spring
  = Operational
  | Damaged
  | Unknown
  deriving (Eq, Ord, Show)

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

-- unfoldRecord :: ([Spring], [Int]) -> ([Spring], [Int])
unfoldRecord n (record, counts) =
  (unfold [Unknown] record, unfold [] counts)
  where
    unfold splitter = intercalate splitter . replicate n

splitOn :: String -> Char -> [String]
splitOn text delimiter
  | null text = []
  | otherwise =
      firstPart : splitOn (drop 1 rest) delimiter
  where
    (firstPart, rest) = break (== delimiter) text

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- (damaged spring counts, last spring (determines state machine status), record)
type ArrangementState = ([Int], Spring, [Spring])

type Arrangements = Map ArrangementState Int

-- Computes the number of solutions for a "spring arrangement state" defined by
-- the scrambled record and the counts of damaged chunks.
-- Uses State to keep a "cache" of computed solutions to avoid repetitive work
-- when the number of solutions is too big.
solve :: ArrangementState -> State Arrangements Int
solve context = do
  arrangements <- get
  case M.lookup context arrangements of
    -- return a computed solution if there is one
    Just solutionCount -> pure solutionCount
    -- compute a solution and store it in state
    Nothing -> do
      solutionCount <- go context
      modify (M.insert context solutionCount)
      pure solutionCount
  where
    -- Arrived to the end of a record while chunking up aall counts---a valid
    -- solution.
    go ([], _, []) = pure 1
    -- Drop a zero count when at the end to be able to finish the calculation
    go ([0], prev, []) = solve ([], prev, [])
    -- Encountering an operational spring right after a group of damaged
    -- springs ended; we can continue as we correctly found a group
    go (0 : cs, _, Operational : springs) = solve (cs, Operational, springs)
    go (0 : cs, _, Damaged : springs) = pure 0
    -- Encountered a damaged, lower the current count and continue on the rest
    go (c : cs, _, Damaged : springs) = solve (c - 1 : cs, Damaged, springs)
    -- Encountered an operational spring in-between other operational springs
    go (cs, Operational, Operational : springs) = solve (cs, Operational, springs)
    -- Branch state when we find an "unknown" spring
    go (cs, previous, Unknown : springs) = do
      v1 <- solve (cs, previous, Damaged : springs)
      v2 <- solve (cs, previous, Operational : springs)
      pure $ v1 + v2
    -- Catch-all for invalid states
    go _ = pure 0

countSolutions :: [([Spring], [Int])] -> Int
countSolutions records =
  sum counts
  where
    counts = evalState (traverse traverseRecord records) M.empty
    traverseRecord (record, count) = solve (counts, Operational, record)

main :: IO ()
main = do
  stdin <- getContents
  let records = readRecords stdin
      solution1 = countSolutions records
      solution2 = countSolutions . map (unfoldRecord 5) $ records
  print solution1
  print solution2
