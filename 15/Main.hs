{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Char
import Data.List.Split

hash :: String -> Int
hash =
  foldl character 0
  where
    character value c = (value + ord c) * 17 `mod` 256

readInput :: String -> [String]
readInput = splitOn "," . filter (not . isSpace)

main :: IO ()
main = do
  putStrLn "day 15"
  stdin <- getContents
  let verificationNumber = sum . map hash . readInput $ stdin
  print verificationNumber
