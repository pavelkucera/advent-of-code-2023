{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)
import Data.Maybe (fromJust, fromMaybe)

parseLine1 :: String -> Int
parseLine1 line =
  let
    firstDigit = findFirstDigit line
    lastDigit = findLastDigit line
  in read $ firstDigit ++ lastDigit
  where
    findFirstDigit = take 1 . dropWhile (not . isDigit) 
    findLastDigit = findFirstDigit . reverse

parseLine :: String -> Int
parseLine line =
  let
    firstDigit = fromJust $ findFirstDigit line
    lastDigit = fromJust $ findLastDigit line
  in read $ firstDigit ++ lastDigit
  where
    findFirstDigit :: String -> Maybe String
    findFirstDigit line =
      case takeDigit line of
        Just x -> Just x
        Nothing -> findFirstDigit . tail $ line

    findLastDigit :: String -> Maybe String
    findLastDigit [] = Nothing
    findLastDigit line =
      case findLastDigit . tail $ line of
        Just x -> Just x
        Nothing -> takeDigit line

takeDigit :: String -> Maybe String
takeDigit (x:_) | isDigit x = Just $ x:[]
takeDigit xs | startsWith xs "one" = Just "1"
takeDigit xs | startsWith xs "two" = Just "2"
takeDigit xs | startsWith xs "three" = Just "3"
takeDigit xs | startsWith xs "four" = Just "4"
takeDigit xs | startsWith xs "five" = Just "5"
takeDigit xs | startsWith xs "six" = Just "6"
takeDigit xs | startsWith xs "seven" = Just "7"
takeDigit xs | startsWith xs "eight" = Just "8"
takeDigit xs | startsWith xs "nine" = Just "9"
takeDigit _ = Nothing

startsWith :: String -> String -> Bool
startsWith line prefix = take (length prefix) line == prefix

parse :: String -> Int
parse = sum . map parseLine . lines

main :: IO ()
main = do
  input <- getContents
  putStrLn . show $ parse input
