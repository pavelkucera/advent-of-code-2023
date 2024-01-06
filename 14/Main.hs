{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.Array
import Data.List

data ReflectorDishRock
  = RoundRock
  | CubeRock
  | Empty
  deriving (Eq)

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

tiltNorth :: Dish -> Dish
tiltNorth grid =
  if grid == tiltedGrid
    then grid
    else tiltNorth tiltedGrid
  where
    tiltedGrid = foldr slide grid $ indices grid
    ((minR, minC), (maxR, maxC)) = bounds grid
    slide :: (Int, Int) -> Dish -> Dish
    slide position g =
      case (g ! position, g ! below position) of
        (Empty, RoundRock) -> g // [(position, RoundRock), (below position, Empty)]
        _ -> g
    below (r, c) = withinBounds (r + 1, c)
    withinBounds (r, c) = (withinRange (minR, maxR) r, withinRange (minC, maxC) c)
    withinRange (rangeMin, rangeMax) value = max rangeMin (min value rangeMax)

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
      tilted = tiltNorth grid
  print $ calculateLoad tilted
