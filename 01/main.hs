{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)

parseLine :: String -> Int
parseLine line =
  let
    firstDigit = findFirstDigit line
    lastDigit = findLastDigit line
  in read $ firstDigit ++ lastDigit
  where
    findFirstDigit = take 1 . dropWhile (not . isDigit) 
    findLastDigit = findFirstDigit . reverse

parse :: String -> Int
parse = sum . map parseLine . lines

main :: IO ()
main = do
  input <- getContents
  putStrLn . show $ parse input
