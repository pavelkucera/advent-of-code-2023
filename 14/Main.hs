{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Array
import Data.List
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Tuple
import Debug.Trace

data ReflectorDishRock
  = RoundRock
  | CubeRock
  | Empty
  deriving (Eq, Ord)

data Direction = North | West | South | East deriving (Eq, Ord, Show)

type Grid a = Array (Int, Int) a

type Dish = Grid ReflectorDishRock

instance Show ReflectorDishRock where
  show RoundRock = "O"
  show CubeRock = "#"
  show Empty = "."

readGrid :: String -> Dish
readGrid input =
  array ((1, 1), (rows, columns)) $ concat values
  where
    values = zipWith (\r line -> zip [(r, c) | c <- [1 ..]] (map rock line)) [1 ..] (lines input)
    rows = length values
    columns = length . head $ values
    rock '.' = Empty
    rock '#' = CubeRock
    rock 'O' = RoundRock

printGrid :: Dish -> String
printGrid grid =
  intercalate "\n" [concat [show (grid ! (r, c)) | c <- [minC .. maxC]] | r <- [minR .. maxR]]
  where
    ((minR, minC), (maxR, maxC)) = bounds grid

tilt :: Dish -> Direction -> Dish
tilt grid direction =
  -- Go through "outer blocks" and slide rocks within them; the notion of an
  -- "outer block" is either rows or columns of the grid, depending on the
  -- direction of the slide. For understanding, imagine it as "rows".
  foldl slideWithinBlock grid outerBlocks
  where
    ((minR, minC), (maxR, maxC)) = bounds grid
    columns = [minC .. maxC]
    rows = [minR .. maxR]
    -- Determine if we slide within rows or columns, and determine the
    -- positioning function. When sliding within columns, the produced inner
    -- and outer positions do not match how the Array is structured compared to
    -- the "inner"/"outer" blocks and the position needs to be swapped.
    (outerBlocks, innerBlocks, position) =
      case direction of
        North -> (columns, rows, id)
        South -> (columns, reverse rows, id)
        East -> (rows, reverse columns, swap)
        West -> (rows, columns, swap)
    -- Do the sliding by walking through every cell within a given block. When
    -- sliding within a column we go row by row. Outer block is then the column
    -- and inner block is a row. Vice versa for sliding within a row.
    slideWithinBlock grid outerBlock =
      fst $ foldl (slide outerBlock) (grid, head innerBlocks) (tail innerBlocks)
    -- Slide, keeping the position of the last empty space within the
    -- "innerBlock" part of the accumulator.
    slide outerBlock (grid, innerBlock) nextInnerBlock =
      let currentPosition = position (innerBlock, outerBlock)
          nextPosition = position (nextInnerBlock, outerBlock)
       in case (grid ! currentPosition, grid ! nextPosition) of
            (Empty, RoundRock) -> (grid // [(currentPosition, RoundRock), (nextPosition, Empty)], innerBlock + signum (nextInnerBlock - innerBlock))
            (Empty, Empty) -> (grid, innerBlock)
            _ -> (grid, nextInnerBlock)

spin grid n =
  foldl tilt grid (take (4 * n) $ cycle [North, West, South, East])

calculateLoad :: Dish -> Int
calculateLoad grid =
  foldr load 0 (indices grid)
  where
    ((minR, minC), (maxR, maxC)) = bounds grid
    load index@(row, _) counter =
      counter + case grid ! index of
        RoundRock -> maxR - (row - minR)
        _ -> 0

main :: IO ()
main = do
  stdin <- getContents
  let grid = readGrid stdin
      tilted = tilt grid North
      spun1 = spin grid 3
  print $ calculateLoad tilted
  putStrLn . printGrid $ tilted
  putStrLn ""
  putStrLn . printGrid $ spun1
