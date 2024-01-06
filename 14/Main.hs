{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List

data ReflectorDishRock
  = RoundRock
  | CubeRock
  | Empty
  deriving (Eq)

instance Show ReflectorDishRock where
  show RoundRock = "O"
  show CubeRock = "#"
  show Empty = "."

type ReflectorDish = [[ReflectorDishRock]]

readGrid :: String -> ReflectorDish
readGrid input =
  map (map rock) $ lines input
  where
    rock '.' = Empty
    rock '#' = CubeRock
    rock 'O' = RoundRock

printGrid :: ReflectorDish -> String
printGrid = intercalate "\n" . map (concatMap show)

tiltNorth :: ReflectorDish -> ReflectorDish
tiltNorth grid =
  if grid == tilt grid
    then grid
    else tiltNorth $ tilt grid
  where
    tilt = foldr slide []
    slide above [] = [above]
    slide above (below : layers) =
      let (tiltedAbove, tiltedBelow) = unzip $ zipWith slideRock above below
       in tiltedAbove : tiltedBelow : layers
    slideRock Empty RoundRock = (RoundRock, Empty)
    slideRock above below = (above, below)

calculateLoad :: ReflectorDish -> Int
calculateLoad grid =
  sum $ zipWith load (reverse roundRocks) [1 ..]
  where
    roundRocks = map (filter (== RoundRock)) grid
    load layer n = sum $ map (const n) layer

main :: IO ()
main = do
  stdin <- getContents
  let grid = readGrid stdin
      tilted = tiltNorth grid
  print $ calculateLoad tilted
