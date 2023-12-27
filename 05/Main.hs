module Main where

import Control.Monad (void, when)
import Data.Char (isAlpha)
import Data.List (foldl')
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, eol, string)

-- (destination start, source start, range length)
type Transformation = (Int, Int, Int)

type Almanac = ([Int], [[Transformation]])

type Input = Parsec Void String

transformInRange s (destStart, sourceStart, rangeLength) =
  if sourceStart <= s && s < sourceStart + rangeLength
    then Just $ s - sourceStart + destStart
    else Nothing

-- transform :: [Transformation] -> Int -> Int
transformThroughLayer ts s =
  case mapMaybe (transformInRange s) ts of
    [] -> s
    [s'] -> s'
    _ -> error "Multiple transformations match the same value"

transformSeed ls s =
  foldl' (flip transformThroughLayer) s ls

closestSeedLocation :: Almanac -> Int
closestSeedLocation (seeds, layers) = minimum locations
  where
    locations = map (transformSeed layers) seeds

closestSeedRangeLocation :: Almanac -> Int
closestSeedRangeLocation (seedsRanges, layers) = minimum locations
  where
    locations = map (transformSeed layers) seeds
    seeds = concatMap (uncurry enumFromTo) $ pairs seedsRanges
    -- assuming we check for even number of seed numbers
    pairs [] = []
    pairs (s : e : rs) = (s, s + e - 1) : pairs rs

-- Run a parser and "eat" any following whitespace
lexeme :: Input a -> Input a
lexeme p = p <* many space
  where
    space = char ' '

symbol :: String -> Input ()
symbol s = lexeme . void $ string s

keyword :: String -> Input ()
keyword s = lexeme $ do
  _ <- string s
  notFollowedBy alphaNumChar
  pure ()

numberP :: Input Int
numberP = read <$> some digitChar

almanacP :: Input Almanac
almanacP = do
  keyword "seeds"
  symbol ":"
  seeds <- lexeme $ numberP `sepBy` char ' '
  _ <- some eol
  layers <- mapBlockP `sepEndBy` some eol
  eof
  pure (seeds, layers)

mapBlockP :: Input [Transformation]
mapBlockP = do
  _ <- some . satisfy $ \c -> isAlpha c || c == '-'
  _ <- char ' '
  keyword "map"
  symbol ":"
  _ <- eol
  transformationP `sepEndBy` eol
  where
    transformationP = do
      destinationStart <- lexeme numberP
      sourceStart <- lexeme numberP
      rangeLength <- lexeme numberP
      pure (destinationStart, sourceStart, rangeLength)

main :: IO ()
main = do
  stdin <- getContents
  almanac <- case parse almanacP "stdin" stdin of
    Left e -> fail . errorBundlePretty $ e
    Right a -> pure a

  when (length (fst almanac) `mod` 2 /= 0) (fail "expecting an even number of seed numbers to define ranges")
  print $ closestSeedLocation almanac
  print $ closestSeedRangeLocation almanac
