{-# LANGUAGE TupleSections #-}

module Main where

import Data.Array
import Debug.Trace

data Direction = Up' | Down' | Left' | Right'
  deriving (Eq, Show)

type Grid a = Array (Int, Int) a

data Space
  = Empty
  | Mirror
  | Splitter
  deriving (Eq, Show)

data Light
  = Light
  deriving (Eq, Show)

type Contraption = Grid (Char, [Direction])

passLight :: Contraption -> ((Int, Int), Direction) -> Contraption
passLight grid start =
  go grid [start]
  where
    go grid [] = grid
    go grid ((position, direction) : beams)
      | not (withinBounds position) = go grid beams
      | otherwise =
          let (space, fieldBeams) = grid ! position
           in if direction `elem` fieldBeams
                then go grid beams
                else go (grid // [(position, (space, direction : fieldBeams))]) (next space position direction ++ beams)
    next '.' position direction = [continue position direction]
    next '\\' position Up' = [continue position Left']
    next '\\' position Down' = [continue position Right']
    next '\\' position Left' = [continue position Up']
    next '\\' position Right' = [continue position Down']
    next '/' position Up' = [continue position Right']
    next '/' position Down' = [continue position Left']
    next '/' position Left' = [continue position Down']
    next '/' position Right' = [continue position Up']
    next '-' position Right' = [continue position Right']
    next '-' position Left' = [continue position Left']
    next '-' position _ = [continue position Left', continue position Right']
    next '|' position Up' = [continue position Up']
    next '|' position Down' = [continue position Down']
    next '|' position _ = [continue position Up', continue position Down']
    continue position direction = (move position direction, direction)
    move (y, x) direction =
      case direction of
        Up' -> (y - 1, x)
        Down' -> (y + 1, x)
        Left' -> (y, x - 1)
        Right' -> (y, x + 1)
    ((minY, minX), (maxY, maxX)) = bounds grid
    withinBounds (y, x) = inRange (minY, maxY) y && inRange (minX, maxX) x

energized :: Contraption -> Int
energized =
  sum . fmap isEnergized
  where
    isEnergized (_, []) = 0
    isEnergized (_, _) = 1

readInput :: String -> Contraption
readInput s =
  listArray ((1, 1), (rowCount, columnCount)) $ map (,[]) $ concat rows
  where
    rows = lines s
    rowCount = length rows
    columnCount = length $ head rows

main :: IO ()
main = do
  putStrLn "day 16"
  stdin <- getContents
  let contraption = readInput stdin
      litUpContraption = passLight contraption ((1, 1), Right')
      (_, (maxRows, maxColumns)) = bounds contraption
      possibleStartingPoints =
        concat [[((y, 1), Right'), ((y, maxColumns), Left')] | y <- [1 .. maxRows]]
          ++ concat [[((1, x), Down'), ((maxRows, x), Up')] | x <- [1 .. maxColumns]]
      maxEnergized = maximum $ map (energized . passLight contraption) possibleStartingPoints
  print $ energized litUpContraption
  print maxEnergized
