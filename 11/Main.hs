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
  filter ((== Galaxy) . snd) $ assocs grid

distance grid from to@(tx, ty) =
  go from 0
  where
    go current counter
      | current == to = counter
      | otherwise =
          let direction = nextDirection current
              next = move current direction
           in go next (counter + size current direction)
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
    size (x, y) direction
      | direction `elem` [U, D] = if all (== Empty) (row grid y) then 1000000 else 1
      | direction `elem` [R, L] = if all (== Empty) (column grid x) then 1000000 else 1

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x1 : xs) =
  [(x1, x2) | x2 <- xs] ++ pairs xs

transpose :: Grid a -> Grid a
transpose grid =
  array (newOrigin, newEnd) [((y, x), grid ! (x, y)) | y <- [1 .. fst newEnd], x <- [1 .. snd newEnd]]
  where
    (origin, end) = bounds grid
    newOrigin = swap origin
    newEnd = swap end

expand =
  transpose . double . transpose . double

double grid =
  listArray (origin, newEnd) $ concatMap concat doubledRows
  where
    doubledRows = map (doubleEmpty . row grid) [1 .. maxY]
    (origin, (maxY, maxX)) = bounds grid
    newEnd = (sum . map length $ doubledRows, maxX)
    doubleEmpty xs =
      if all (== Empty) xs
        then replicate 2 xs
        else [xs]

row grid n =
  map (grid !) (indices)
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
      gs = map (swap . fst) $ galaxies grid
      galaxyPairs = pairs gs
      distances = map (uncurry (distance grid)) galaxyPairs
  print $ sum distances
