module Main where

import Data.Array
import Data.Tuple
import Debug.Trace

type Grid a = Array (Int, Int) a

type Position = (Int, Int)

data Direction = U | D | L | R
  deriving (Eq, Show)

data Space
  = Galaxy
  | Empty
  deriving (Eq, Show)

readGrid s = listArray ((1, 1), (height, width)) . concatMap (map r) $ splitInput
  where
    splitInput = lines s
    height = length splitInput
    width = length . head $ splitInput
    r '.' = Empty
    r '#' = Galaxy

printGrid grid =
  unlines [[p $ grid ! (y, x) | x <- [1 .. maxX]] | y <- [1 .. maxY]]
  where
    (_, (maxY, maxX)) = bounds grid
    p Empty = '.'
    p Galaxy = '#'

galaxies grid =
  map (swap . fst) . filter ((== Galaxy) . snd) $ assocs grid

distance grid from to@(tx, ty) =
  go from 0
  where
    go current counter
      | current == to = counter
      | otherwise =
          let direction = nextDirection current
              next = move current direction
           in go next (counter + size current direction)
    -- since we can only go up, left, right and down, and each of these step
    -- have length 1, it does not matter if the path is a "direct" path
    -- (which would normally be shorter) or a path along the rectangle defined
    -- by the two points
    nextDirection (x, y) =
      case (compare x tx, compare y ty) of
        (_, LT) -> D
        (_, GT) -> U
        (LT, _) -> R
        (GT, _) -> L
    move (x, y) direction =
      case direction of
        U -> (x, y - 1)
        D -> (x, y + 1)
        L -> (x - 1, y)
        R -> (x + 1, y)
    -- simulates expanding space
    size (x, y) direction =
      if all (== Empty) space
        then 1000000 -- the "expand empty space" coefficient
        else 1
      where
        space
          | direction `elem` [U, D] = row grid y
          | direction `elem` [L, R] = column grid x

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x1 : xs) =
  [(x1, x2) | x2 <- xs] ++ pairs xs

row grid n =
  map (grid !) indices
  where
    indices = [(n, x) | x <- [minX .. maxX]]
    ((_, minX), (_, maxX)) = bounds grid

column grid n =
  map (grid !) indices
  where
    indices = [(y, n) | y <- [1 .. maxY]]
    (_, (maxY, _)) = bounds grid

main :: IO ()
main = do
  stdin <- getContents
  let grid = readGrid stdin
      galaxyPairs = pairs $ galaxies grid
      distances = map (uncurry (distance grid)) galaxyPairs
  print $ sum distances
