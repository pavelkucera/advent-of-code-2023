{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Data.Functor (($>))
import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

data Instruction
  = L
  | R
  deriving (Eq, Show)

type Input = Parsec Void String

type NetworkMap = Map String (String, String)

-- Part two seems to be a play at finding a cycle on the path between start
-- nodes and end nodes. Solution would be to find those cycles and then find
-- least common multiple.

stepCount :: NetworkMap -> [Instruction] -> Int
stepCount network baseInstructions = go "AAA" (cycle baseInstructions) 0
  where
    go "ZZZ" _ c = c
    go node (i : is) c =
      go (choose i $ network M.! node) is (c + 1)
    choose L = fst
    choose R = snd

networkMapP :: Input ([Instruction], NetworkMap)
networkMapP = do
  is <- many instructionP
  some eol
  nodes <- nodeP `sepEndBy` eol
  eof
  pure (is, M.fromList nodes)

instructionP :: Input Instruction
instructionP =
  choice
    [ char 'L' $> L,
      char 'R' $> R
    ]

nameP :: Input String
nameP = count 3 alphaNumChar

nodeP :: Input (String, (String, String))
nodeP = do
  n <- lexeme nameP
  symbol "="
  symbol "("
  l <- lexeme nameP
  symbol ","
  r <- lexeme nameP
  symbol ")"
  pure (n, (l, r))
  where
    lexeme p = p <* many (char ' ')
    symbol s = lexeme $ string s

main :: IO ()
main = do
  stdin <- getContents
  (instructions, networkMap) <- case parse networkMapP "stdin" stdin of
    Left e -> fail . errorBundlePretty $ e
    Right nm -> pure nm

  print $ stepCount networkMap instructions
