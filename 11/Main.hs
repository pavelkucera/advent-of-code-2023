module Main where

import Data.Array
import Data.Tuple

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
  putStr $ unlines [[p $ grid ! (y, x) | x <- [1 .. maxX]] | y <- [1 .. maxY]]
  where
    (_, (maxY, maxX)) = bounds grid
    p Empty = '.'
    p Galaxy = '#'

galaxies grid =
  filter ((== Galaxy) . snd) $ assocs grid

distance (x1, y1) (x2, y2) =
  sum . map abs $ [x1 - x2, y1 - y2]

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
  map (grid !) indices
  where
    indices = [(n, x) | x <- [1 .. maxX]]
    (_, (_, maxX)) = bounds grid

column grid n =
  map (grid !) indices
  where
    indices = [(y, n) | y <- [1 .. maxY]]
    (_, (maxY, _)) = bounds grid

main :: IO ()
main = do
  stdin <- getContents
  let grid = expand $ readGrid stdin
      gs = map fst $ galaxies grid
      galaxyPairs = pairs gs
      distances = map (uncurry distance) galaxyPairs
  print $ sum distances
