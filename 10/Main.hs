module Main where

import Data.Array
import Data.Bifunctor
import Data.Maybe
import Data.Tuple
import Debug.Trace

data PipeDirection = NS | WE | NE | NW | SW | SE
  deriving (Eq, Show)

data Direction = U | D | L | R
  deriving (Eq, Show)

data Tile
  = Ground
  | Start
  | Pipe PipeDirection
  deriving (Eq, Show)

type Position = (Int, Int)

type Grid a = Array (Int, Int) a

type Loop = (Position, Grid Tile)

-- not quite satisfied with this one :)

furthestPointDistance grid origin =
  ceiling . (/ 2) . realToFrac . length . head $ paths
  where
    paths = filter (not . null) . map (\n -> followPipe origin n []) $ neighboursOf grid origin
    followPipe previous position visited =
      case grid ! swap position of
        Ground -> []
        Start -> visited
        Pipe pipeDirection ->
          case moveInPipe position pipeDirection (movementFromTo previous position) of
            Nothing -> []
            Just next ->
              followPipe position next (position : visited)

neighboursOf grid position =
  filter (withinBounds grid) . map (move position) $ [U, D, L, R]

el grid position
  | withinBounds grid position = Just $ grid ! swap position
  | otherwise = Nothing

moveInPipe position pipeDirection direction =
  move position <$> case (pipeDirection, direction) of
    (NS, U) -> Just U
    (NS, D) -> Just D
    (WE, R) -> Just R
    (WE, L) -> Just L
    (NE, D) -> Just R
    (NE, L) -> Just U
    (SE, U) -> Just R
    (SE, L) -> Just D
    (NW, D) -> Just L
    (NW, R) -> Just U
    (SW, U) -> Just L
    (SW, R) -> Just D
    _ -> Nothing

move (x, y) direction =
  case direction of
    U -> (x, y - 1)
    D -> (x, y + 1)
    L -> (x - 1, y)
    R -> (x + 1, y)

-- works properly only on direct neighbours
movementFromTo (x1, y1) (x2, y2) =
  case (compare x1 x2, compare y1 y2) of
    (EQ, LT) -> D
    (EQ, GT) -> U
    (GT, EQ) -> L
    (LT, EQ) -> R

withinBounds :: Grid a -> Position -> Bool
withinBounds grid (x, y) =
  minX <= x && x <= maxX && minY <= y && y <= maxY
  where
    ((minY, minX), (maxY, maxX)) = bounds grid

gridFromString :: String -> Grid Tile
gridFromString s = listArray ((1, 1), (height, width)) . concatMap (map tile) $ lines s
  where
    height = length $ lines s
    width = length . head $ lines s
    tile '.' = Ground
    tile 'S' = Start
    tile '|' = Pipe NS
    tile '-' = Pipe WE
    tile 'L' = Pipe NE
    tile 'J' = Pipe NW
    tile '7' = Pipe SW
    tile 'F' = Pipe SE

-- origin :: Grid Tile -> (Int, Int)
origin g = swap . fst . head . filter ((== Start) . snd) $ assocs g

main :: IO ()
main = do
  stdin <- getContents
  let grid = gridFromString stdin
  print $ furthestPointDistance grid (origin grid)
  print $ enclosed grid (origin grid)
