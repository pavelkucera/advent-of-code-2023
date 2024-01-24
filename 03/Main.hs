module Main where

import Data.Char (isDigit)
import Data.List.Split
import Data.Matrix

type Schematics = Matrix Char

readInput :: String -> Schematics
readInput = fromLists . lines

isSymbol :: Char -> Bool
isSymbol character =
  not (isDigit character) && character /= '.'

findParts schematics =
  concat . toList $ mapPos findNeighbouringParts schematics
  where
    findNeighbouringParts position c
      | isSymbol c = neighbouringNumbers schematics position
      | otherwise = []

findGears schematics =
  filter (not . null) . toList $ mapPos neighbouringPartNumbers schematics
  where
    neighbouringPartNumbers position c
      | c == '*' = keepGears $ neighbouringNumbers schematics position
      | otherwise = []
    keepGears ns
      | length ns /= 2 = []
      | otherwise = ns

neighbouringNumbers :: Schematics -> (Int, Int) -> [Int]
neighbouringNumbers schematics (y, x) =
  map read . concatMap (filter (not . null) . splitWhen (not . isDigit)) $ neighbouringBlocks
  where
    -- All the neighbours and any horizontally adjacent numbers; This includes
    -- the value on the symbol itself, which works out in this execercise as
    -- that is never a digit.
    neighbouringBlocks =
      [toList $ submatrix y' y' (minDigitX y' minX) (maxDigitX y' maxX) schematics | y' <- [minY .. maxY]]
    minY = max (y - 1) 1
    maxY = min (y + 1) (nrows schematics)
    minX = max (x - 1) 1
    maxX = min (x + 1) (ncols schematics)
    minDigitX y x
      | x <= 1 = 1
      | isDigit (schematics ! (y, x)) = minDigitX y (x - 1)
      | otherwise = x + 1
    maxDigitX y x
      | x >= ncols schematics = ncols schematics
      | isDigit (schematics ! (y, x)) = maxDigitX y (x + 1)
      | otherwise = x - 1

main :: IO ()
main = do
  putStrLn "day 03"
  stdin <- getContents
  let schematics = readInput stdin
      partList = findParts schematics
      gearList = findGears schematics
  print $ sum partList
  print $ sum $ map product gearList
