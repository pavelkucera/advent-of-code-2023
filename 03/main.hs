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

-- does not implement part two, as the data structure I chose would not be great

data EngineSymbol
  = Number Int
  | Period
  | Symbol (Maybe Char)
  deriving (Eq, Show)

type EngineSchematics = IntMap (IntMap EngineSymbol)

type Input = Parsec Void String

sumPartNumbers :: EngineSchematics -> Int
sumPartNumbers = IM.foldr (\r s -> sum r + s) 0 . collectPartNumbers

collectPartNumbers :: EngineSchematics -> IntMap [Int]
collectPartNumbers s = IM.map collapsePartNumbers taggedSchematics
  where
    taggedSchematics = IM.mapWithKey tag s
    tag y = IM.mapWithKey (\x v -> (v, neighboursSymbol s (x, y)))

-- Combines replicated part numbers into one, keeping their "is a neighbour of
-- a symbol" tag and extracts the numbers from part numbers
collapsePartNumbers :: IntMap (EngineSymbol, Bool) -> [Int]
collapsePartNumbers = mapMaybe (number . fst) . filter snd . IM.foldr' combine []
  where
    number (Number n) = Just n
    number _ = Nothing
    combine symbol [] = [symbol]
    combine symbol (previous : tail) =
      if fst symbol == fst previous
        then (fst symbol, snd symbol || snd previous) : tail
        else symbol : previous : tail

neighboursSymbol :: EngineSchematics -> (Int, Int) -> Bool
neighboursSymbol s p = any isSymbol $ neighboursOf s p
  where
    isSymbol (Symbol _) = True
    isSymbol _ = False

neighboursOf :: EngineSchematics -> (Int, Int) -> [EngineSymbol]
neighboursOf s (x, y) = elementsAt neighbouringPositions
  where
    neighbouringPositions = filter (/= (x, y)) [(x + x', y + y') | x' <- [-1 .. 1], y' <- [-1 .. 1]]
    elementsAt = mapMaybe (symbolAt s)

symbolAt :: EngineSchematics -> (Int, Int) -> Maybe EngineSymbol
symbolAt s (x, y) = IM.lookup y s >>= IM.lookup x

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
    <|> (char '*' $> Symbol (Just '*'))
    <|> (oneOf "$#-*+%&=/@" $> Symbol Nothing)

parseSchematics :: String -> IO EngineSchematics
parseSchematics stdin = do
  case parse schematics "stdin" stdin of
    Left e -> error . errorBundlePretty $ e
    Right s -> pure s

main :: IO ()
main = do
  stdin <- getContents
  schematics <- parseSchematics stdin
  let sumOfPartNumbers = sumPartNumbers schematics
  print sumOfPartNumbers
