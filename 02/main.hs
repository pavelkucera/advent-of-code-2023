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

isValidGame :: Bag -> Game -> Bool
isValidGame b g = allValid rounds
  where
    allValid = and . map isValidRound
    rounds = gameRounds g
    isValidRound round = M.isSubmapOfBy (<=) round b

-- Use a parser and "eat" following whitespace
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

number :: Input Int
number = lexeme $ read <$> some digitChar

cubeInput :: Input (Color, Int)
cubeInput = do 
  count <- number
  color <- color
  pure (color, count)
    where
      color = lexeme $
            string "blue"  $> Blue
        <|> string "green" $> Green
        <|> string "red"   $> Red

-- If there are multiple ocurrences of the same color, the parser uses the last
-- value rather than combining
roundInput :: Input Bag
roundInput = M.fromList <$> cubeInput `sepBy` (symbol ",")

gameInput :: Input Game
gameInput = do
  keyword "Game"
  id <- number
  symbol ":"
  rounds <- roundInput `sepBy` (symbol ";")
  pure $ Game id rounds

input :: Input [Game]
input = do
  games <- lexeme $ gameInput `sepEndBy` (newline)
  skipMany (satisfy isSpace)
  eof
  pure games

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

  let validGames = filter (isValidGame bagWithCubes) games
  let idsNumber = foldr (\g n -> gameId g + n) 0 validGames
  putStr . show $ idsNumber
