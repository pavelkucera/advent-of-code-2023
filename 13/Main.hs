{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Array
import Data.Tuple
import Debug.Trace

data Terrain
  = Ash
  | Rock
  deriving (Eq, Show)

type Grid a = Array (Int, Int) a

summarize :: Grid Terrain -> Int
summarize grid =
  rowsSummary + columnsSummary
  where
    ((minR, minC), (maxR, maxC)) = bounds grid
    rowsSummary = sum [scoreRow r | r <- [minR .. maxR - 1]]
    columnsSummary = sum [scoreColumn c | c <- [minC .. maxC - 1]]
    -- Row's score is the number of lines above the mirror line; if the line is
    -- after row number N, N is the number of rows above the line
    scoreRow r =
      if isMirrorLineAfterRow grid r
        then r
        else 0
    -- Columns's score is 100 * the number of columns left of the mirror line;
    -- if the line is after row number n, N is the number of relevant columns
    scoreColumn c =
      if isMirrorLineAfterColumn grid c
        then 100 * c
        else 0

-- Checks if there is a mirror line after a row R; does this by looking at all
-- the rows after R, making sure that they match a corresponding row from before
-- R
isMirrorLineAfterRow :: (Eq a) => Grid a -> Int -> Bool
isMirrorLineAfterRow grid r =
  all rowsMirror shouldMirrorPairs
  where
    ((minR, minC), (maxR, maxC)) = bounds grid
    shouldMirrorPairs = [(n, r + 1 - (n - r)) | n <- [r + 1 .. maxR]]
    rowsMirror (r1, r2)
      -- if either of the rows are outside of the grid, the pair matches
      -- automatically
      | not $ all (inRange (minR, maxR)) [r1, r2] = True
      -- otherwise all the corresponding values must match
      | otherwise =
          and [grid ! (r1, x) == grid ! (r2, x) | x <- [minC .. maxC]]

-- Checks if there is a mirror line after a column C; does this by looking at
-- all the columns after C, making sure that they match a corresponding column
-- from before C
isMirrorLineAfterColumn :: (Eq a) => Grid a -> Int -> Bool
isMirrorLineAfterColumn grid c =
  all columnsMirror shouldMirrorPairs
  where
    ((minR, minC), (maxR, maxC)) = bounds grid
    shouldMirrorPairs = [(n, c + 1 - (n - c)) | n <- [c + 1 .. maxC]]
    columnsMirror (c1, c2)
      -- if either of the columns are outside of the grid, the pair matches
      -- automatically
      | not $ all (inRange (minC, maxC)) [c1, c2] = True
      -- otherwise all the corresponding values must match
      | otherwise =
          and [grid ! (y, c1) == grid ! (y, c2) | y <- [minR .. maxR]]

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
      summary = sum $ map summarize terrain
  print summary
