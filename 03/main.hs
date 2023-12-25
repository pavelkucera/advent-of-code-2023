#!/usr/bin/env stack
-- stack script --resolver lts-21.25 --package megaparsec --package containers

import Data.Functor (($>), (<&>))
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Maybe
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char

data EngineSymbol
  = Number Int
  | Period
  | Symbol
  deriving (Eq, Show)

type EngineSchematics = IntMap (IntMap EngineSymbol)

type Input = Parsec Void String

-- can parse the input
-- storing each number at all the positions of its digits
-- need to look at the numbers and see if they neighbour a symbol
--   watch out: this will differ per position of digit
-- and then I need to collapse

collectPartNumbers :: EngineSchematics -> [Int]
collectPartNumbers s = undefined
  where
    -- numbers :: IntMap EngineSymbol -> [Int]
    numbers = IM.mapWithKey $ \y -> curry $ neighboursSymbol s

neighboursSymbol :: EngineSchematics -> (Int, Int) -> Bool
neighboursSymbol s p = any (== Symbol) $ neighboursOf s p

neighboursOf :: EngineSchematics -> (Int, Int) -> [EngineSymbol]
neighboursOf s (x, y) = elementsAt neighbouringPositions
  where
    neighbouringPositions = filter (/= (x, y)) [(x + x', y + y') | x' <- [-1 .. 1], y' <- [-1 .. 1]]
    elementsAt = mapMaybe (symbolAt s)

symbolAt :: EngineSchematics -> (Int, Int) -> Maybe EngineSymbol
symbolAt s (x, y) = IM.lookup x s >>= IM.lookup y

numbered :: [a] -> IntMap a
numbered = IM.fromAscList . zip [0 ..]

schematics :: Input EngineSchematics
schematics = do
  lines <- line `sepEndBy` newline
  eof
  pure . numbered $ lines

line :: Input (IntMap EngineSymbol)
line = numbered . replicateNumbers <$> some symbol
  where
    -- replicates each number engine symbol by its order of magnitude so that we
    -- can access the number on all of its position in the line
    replicateNumbers = concatMap replicateSymbol
    replicateSymbol s@(Number n) = replicate (length . show $ n) s
    replicateSymbol s = [s]

-- Parses a full engine symbol, and a full number as the actual number which messes
-- up positioning; and it needs to be accommodate for
symbol :: Input EngineSymbol
symbol =
  (some digitChar <&> Number . read)
    <|> (char '.' $> Period)
    <|> (oneOf "$#-*" $> Symbol)

-- number :: Input Int
-- number = read <$> some digitChar
--
-- symbol :: Input Char
-- symbol = satisfy $ \c -> c /= '.' && isSymbol c

main :: IO ()
main = do
  putStrLn "Hello, World!"
