{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Array
import Data.List
import Data.Tuple
import Debug.Trace

data Terrain
  = Ash
  | Rock
  deriving (Eq, Show)

type Grid a = Array (Int, Int) a

summarize :: Grid Terrain -> (Grid Terrain -> Int -> Bool) -> (Grid Terrain -> Int -> Bool) -> Int
summarize grid isMirrorLineAfterRow isMirrorLineAfterColumn =
  rowsSummary + columnsSummary
  where
    ((minR, minC), (maxR, maxC)) = bounds grid
    rowsSummary = sum [scoreRow r | r <- [minR .. maxR - 1]]
    columnsSummary = sum [scoreColumn c | c <- [minC .. maxC - 1]]
    scoreRow r =
      if isMirrorLineAfterRow grid r
        then 100 * r
        else 0
    scoreColumn c =
      if isMirrorLineAfterColumn grid c
        then c
        else 0

mirrorLineAfterRowSmudgeCount :: (Eq a) => Grid a -> Int -> Int
mirrorLineAfterRowSmudgeCount grid r =
  sum $ map rowsSmudgeCount shouldMirrorPairs
  where
    ((minR, minC), (maxR, maxC)) = bounds grid
    shouldMirrorPairs = [(n, r + 1 - (n - r)) | n <- [r + 1 .. maxR]]
    rowsSmudgeCount (r1, r2)
      -- if either of the rows are outside of the grid, the pair matches
      -- automatically
      | not $ all (inRange (minR, maxR)) [r1, r2] = 0
      -- otherwise all the corresponding values must match
      | otherwise =
          length . filter not $ [grid ! (r1, x) == grid ! (r2, x) | x <- [minC .. maxC]]

mirrorLineAfterColumnSmudgeCount :: (Eq a) => Grid a -> Int -> Int
mirrorLineAfterColumnSmudgeCount grid c =
  sum $ map columnSmudgeCount shouldMirrorPairs
  where
    ((minR, minC), (maxR, maxC)) = bounds grid
    shouldMirrorPairs = [(n, c + 1 - (n - c)) | n <- [c + 1 .. maxC]]
    columnSmudgeCount (c1, c2)
      -- if either of the rows are outside of the grid, the pair matches
      -- automatically
      | not $ all (inRange (minC, maxC)) [c1, c2] = 0
      -- otherwise all the corresponding values must match
      | otherwise =
          length . filter not $ [grid ! (r, c1) == grid ! (r, c2) | r <- [minR .. maxR]]

-- Reads a single pattern into a terrain grid
readPattern :: String -> Grid Terrain
readPattern input =
  listArray ((1, 1), (rows, columns)) values
  where
    inputLines = lines input
    values = concatMap (map terrain) inputLines
    rows = length inputLines
    columns = length (head inputLines)
    terrain '.' = Ash
    terrain '#' = Rock

-- Reads a list of patterns, interleaved with a pair of newlines, into a list
-- of terrain grids
readTerrain :: String -> [Grid Terrain]
readTerrain input =
  map (readPattern . unlines) $ splitOn (lines input) ""

splitOn :: (Eq a) => [a] -> a -> [[a]]
splitOn list delimiter
  | null list = []
  | otherwise =
      firstPart : splitOn (drop 1 rest) delimiter
  where
    (firstPart, rest) = break (== delimiter) list

main :: IO ()
main = do
  stdin <- getContents
  let terrain = readTerrain stdin
      summary1 = sum $ map summarize1 terrain
      summary2 = sum $ map summarize2 terrain
  print summary1
  print summary2
  where
    summarize1 grid = summarize grid (\g r -> mirrorLineAfterRowSmudgeCount g r == 0) (\g c -> mirrorLineAfterColumnSmudgeCount g c == 0)
    summarize2 grid = summarize grid (\g r -> mirrorLineAfterRowSmudgeCount g r == 1) (\g c -> mirrorLineAfterColumnSmudgeCount g c == 1)
