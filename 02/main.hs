#!/usr/bin/env stack
-- stack script --resolver lts-22.3 --package megaparsec --package containers
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Map (Map)
import Data.Void
import qualified Data.Map.Strict as M
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, newline, string)

data Color
  = Blue
  | Green
  | Red
  deriving (Eq, Ord, Show)

type Bag
  = Map Color Int

data Game
  = Game
    { gameId :: Int
    , gameRounds :: [Bag]
    }
    deriving (Eq, Show)

type Input = Parsec Void String

-- Check if game is possible with the given bag
isPossibleGame :: Bag -> Game -> Bool
isPossibleGame b g = allValid rounds
  where
    allValid = and . map isValidRound
    rounds = gameRounds g
    isValidRound round = M.isSubmapOfBy (<=) round b

-- Get specs of a bag `b` with the minimum number of cubes of each color such
-- that `isPossibleGame (minPossibleBag game) == True
minPossibleBag :: Game -> Bag
minPossibleBag = foldr (M.unionWith max) M.empty . gameRounds

-- The power of a set of cubes is equal to the numbers of red, green, and blue
-- cubes multiplied together
bagPower :: Bag -> Int
bagPower = M.foldr (*) 1

-- Use a parser and "eat" following spaces; use carefully as the grammar does
-- not allow for a lot of spaces
lexeme :: Input a -> Input a
lexeme p = p <* space
  where
    space = skipMany . satisfy $ (== ' ')

symbol :: String -> Input ()
symbol s = lexeme $ string s >> pure ()

-- Normally would need to check that keyword is not followed by an alphanumeric
-- character, but working with a simple grammar here
keyword :: String -> Input ()
keyword = symbol

-- Parse a positive number allowing for leading zeros
number :: Input Int
number = lexeme $ read <$> some digitChar

-- Parse number + color
-- 3 green
-- 50 red
cubeCount :: Input (Color, Int)
cubeCount = do 
  count <- number
  color <- color
  pure (color, count)
    where
      color = lexeme $
            string "blue"  $> Blue
        <|> string "green" $> Green
        <|> string "red"   $> Red

-- Parse cube counts separated by commas
-- 
-- If there are multiple ocurrences of the same color, the parser uses the last
-- value rather than combining
roundInput :: Input Bag
roundInput = M.fromList <$> cubeCount `sepBy` (symbol ",")

-- Parse a line with a full game
gameInput :: Input Game
gameInput = do
  keyword "Game"
  id <- number
  symbol ":"
  rounds <- roundInput `sepBy` (symbol ";")
  pure $ Game id rounds

-- Parse full input (require eof)
input :: Input [Game]
input = do
  games <- lexeme $ gameInput `sepEndBy` (newline)
  skipMany (satisfy isSpace)
  eof
  pure games

-- Parse games, report errors. Not great, but will do for Advent of Code.
parseGames :: String -> IO [Game]
parseGames def = do
  case parse input "stdin" def of
    Left e -> error . errorBundlePretty $ e
    Right g -> pure g

main :: IO ()
main = do
  let bagWithCubes = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

  stdin <- getContents
  games <- parseGames stdin

  let validGames = filter (isPossibleGame bagWithCubes) games
  let validGamesIds = map gameId validGames
  let validGamesIdsSum = sum validGamesIds
  putStrLn . ("Sum of IDs of possible games: " ++) . show $ validGamesIdsSum

  let minPossibleBags = map minPossibleBag games
  let powers = map bagPower minPossibleBags
  let powersSum = sum powers
  putStrLn . ("Sum of min possible bag powers: " ++) . show $ powersSum
