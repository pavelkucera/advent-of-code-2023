module Main where

import Data.Char
import Data.List.Split

data BoxOperation
  = RemoveLens String
  | FocusLens String Int
  deriving (Eq, Show)

hash :: String -> Int
hash =
  foldl character 0
  where
    character hashValue c = (hashValue + ord c) * 17 `mod` 256

readInput :: String -> [String]
readInput = splitOn "," . filter (not . isSpace)

readOperation :: String -> BoxOperation
readOperation label =
  case break (== '=') label of
    (name, []) -> RemoveLens (init name)
    (name, focalLength) -> FocusLens name (read . tail $ focalLength)

focus :: [BoxOperation] -> [[(String, Int)]]
focus =
  foldl perform (replicate 256 [])
  where
    perform boxes (RemoveLens label) = dropLens label boxes
    perform boxes (FocusLens label focalStrength) = focusLens (label, focalStrength) boxes

dropLens label boxes =
  prefix ++ box' : drop 1 suffix
  where
    (prefix, suffix) = splitAt (hash label) boxes
    box = head suffix
    box' = filter ((/= label) . fst) box

focusLens lens@(label, _) boxes =
  prefix ++ box' : drop 1 suffix
  where
    (prefix, suffix) = splitAt (hash label) boxes
    box' = upsert (head suffix) lens

upsert lenses lens@(label, _) =
  case break ((== label) . fst) lenses of
    (prefix, []) -> prefix ++ [lens]
    (prefix, _ : suffix) -> prefix ++ lens : suffix

focusingPower :: [[(String, Int)]] -> Int
focusingPower =
  foldl collect 0 . index
  where
    index = zip [1 ..]
    collect power (boxN, box) =
      power + sum (map (boxN *) $ boxPower box)
    boxPower = zipWith (*) [1 ..] . map snd

main :: IO ()
main = do
  putStrLn "day 15"
  stdin <- getContents
  let input = readInput stdin
      verificationNumber = sum $ map hash input
      hashmap = focus $ map readOperation input
  print verificationNumber
  print $ focusingPower hashmap
