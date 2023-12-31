#!/usr/bin/env stack
-- stack script --resolver lts-21.25 --package megaparsec --package containers

import Control.Monad (void)
import Data.Functor (($>), (<&>))
import Data.Maybe
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char

data LotteryCard = Card
  { cardId :: Int,
    cardWinningNumbers :: [Int],
    cardNumbers :: [Int]
  }
  deriving (Eq, Show)

type Input = Parsec Void String

-- scoring rules:
-- You have to figure out which of the numbers you have appear in the list of
-- winning numbers. The first match makes the card worth one point and
-- each match after the first doubles the point value of that card.
points :: LotteryCard -> Int
points (Card _ wns ns) = multiply . foldr coefficient [] . reverse $ filter (`elem` wns) ns
  where
    multiply [] = 0
    multiply x = product x
    coefficient n [] = [1]
    coefficient _ ns = 2 : ns

-- Gives the count all cards, originals and copies
cardCount cs = length cs + copiesWon cs

-- Gives the number of copies won by the deck
copiesWon cs = winningsOf cs
  where
    winningsOf [] = 0
    winningsOf (c : cs') =
      let winningCount = length . filter (`elem` cardWinningNumbers c) $ cardNumbers c
          copies = take winningCount cs'
       in winningCount + winningsOf copies + winningsOf cs'

-- returns direct winnings of a card
cardsWonBy cs c = take winningCount followingCards
  where
    winningCount = length . filter (`elem` cardWinningNumbers c) $ cardNumbers c
    followingCards = drop (cardId c) cs

-- Run a parser and "eat" any following whitespace
lexeme :: Input a -> Input a
lexeme p = p <* many space
  where
    space = char ' '

symbol :: String -> Input ()
symbol s = lexeme . void $ string s

numberP :: Input Int
numberP = read <$> some digitChar

card :: Input LotteryCard
card = do
  string "Card"
  space
  id <- numberP
  symbol ":"
  winningNs <- numberP `sepEndBy` space
  symbol "|"
  ns <- numberP `sepEndBy` space
  pure (Card id winningNs ns)
  where
    space = some (char ' ')

-- parse full input, expecting end of file
cards :: Input [LotteryCard]
cards = do
  cs <- card `sepEndBy` eol
  eof
  pure cs

parseInput :: String -> IO [LotteryCard]
parseInput stdin = do
  case parse cards "stdin" stdin of
    Left e -> error . errorBundlePretty $ e
    Right cs -> pure cs

main :: IO ()
main = do
  stdin <- getContents
  cards <- parseInput stdin

  let pointWinningsPerCard = map points cards
  let pointWinnings = sum pointWinningsPerCard
  let cardWinnings = cardCount cards
  print pointWinnings
  print cardWinnings
