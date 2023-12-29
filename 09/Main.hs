{-# LANGUAGE ImportQualifiedPost #-}

module Main where

differences :: [Int] -> [Int]
differences [] = []
differences [_] = []
differences (x1 : x2 : xs) = x2 - x1 : differences (x2 : xs)

predictionLayers xs =
  collect xs []
  where
    collect layer layers
      | all (== 0) layer = layers
      | otherwise = collect (differences layer) (layer : layers)

predict xs =
  -- Using 0 as the starting point as for the zero layer that is always the
  -- next value.
  foldl next 0 $ predictionLayers xs
  where
    next previousPrediction layer = previousPrediction + last layer

-- Writing a proper parser felt like too much work for this one :)
parse :: String -> [[Int]]
parse = map parseLine . lines
  where
    parseLine = map read . words

main :: IO ()
main = do
  stdin <- getContents
  let sequences = parse stdin
      predictions = map predict sequences
      history = map (predict . reverse) sequences
  print $ sum predictions
  print $ sum history
