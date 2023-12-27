module Main where

import Control.Monad (void, when)
import Data.Bifunctor (bimap)
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

-- inclusive in its beginning, exclusive in its end
type Interval = (Int, Int)

main :: IO ()
main = do
  stdin <- getContents
  almanac <- case parse almanacP "stdin" stdin of
    Left e -> fail . errorBundlePretty $ e
    Right a -> pure a

  when (odd . length $ fst almanac) (fail "expecting an even number of seed numbers to define ranges")
  print $ closestSeedLocation almanac
  print $ closestSeedRangeLocation almanac

-- Threads a single seed through the almanac
closestSeedLocation :: Almanac -> Int
closestSeedLocation (seeds, layers) = minimum locations
  where
    locations = map (transformSeed layers) seeds

-- Threads seed intervals through the interval working on intervals rather than
-- actual seed numbers to speed it up. Once we get to the result, any of
-- intervals with the lowest beginning should be the overall "closest location"
closestSeedRangeLocation :: Almanac -> Int
closestSeedRangeLocation (seedsRanges, almanac) = minimum . map fst $ locations
  where
    locations = concatMap (`transformIntervalThroughAlmanac` almanac) (ranges seedsRanges)
    -- assuming we check for even number of seed numbers
    ranges [] = []
    ranges (s : e : rs) = (s, s + e) : ranges rs

transformIntervalThroughAlmanac :: Interval -> [[Transformation]] -> [Interval]
transformIntervalThroughAlmanac i a =
  foldl' combine [i] a
  where
    combine is a = concatMap (`transformIntervalThroughLayer` a) is

transformIntervalThroughLayer :: Interval -> [Transformation] -> [Interval]
transformIntervalThroughLayer interval layer =
  transformed ++ untouched
  where
    (transformed, untouched) = foldl' combine ([], [interval]) layer
    combine :: ([Interval], [Interval]) -> Transformation -> ([Interval], [Interval])
    combine (transformed, toTransform) t =
      let (a, b) = unzip $ map (`transformInterval` t) toTransform
       in (concat a ++ transformed, concat b)

transformInterval :: Interval -> Transformation -> ([Interval], [Interval])
transformInterval interval (destinationStart, sourceStart, rangeLength) =
  (map (bimap transformValue transformValue) intersection, remainder)
  where
    (intersection, remainder) = interval `intersect` (sourceStart, sourceStart + rangeLength)
    transformValue x = x - sourceStart + destinationStart

-- Returns ([intersection between i1 and i2], [remainders of i1 that do not intersect i2]
-- not quite beautiful
intersect :: Interval -> Interval -> ([Interval], [Interval])
intersect i1@(b1, e1) i2@(b2, e2)
  -- disjoint intervals
  | e1 <= b2 || e2 <= b1 = ([], [i1])
  -- i1 fully within i2
  | b1 `inInterval` i2 && (e1 - 1) `inInterval` i2 = ([i1], [])
  -- i1 beings within i2 and ends outside
  | b1 `inInterval` i2 = ([(b1, e2)], [(e2, e1)])
  -- i1 ends within i2 and starts outside
  | (e1 - 1) `inInterval` i2 = ([(b2, e1)], [(b1, b2)])
  -- i2 fully within i1
  | b2 `inInterval` i1 && (e2 - 1) `inInterval` i1 = ([i2], [(b1, b2), (e2, e1)])
  -- i2 begins within i1 and ends outside
  | b2 `inInterval` i1 = ([(b2, e1)], [(b1, b2)])
  -- i2 ends within i1 and begins outside
  | (e2 - 1) `inInterval` i1 = ([(b1, e2)], [(e2, e1)])

inInterval :: Int -> Interval -> Bool
inInterval x (b, e) = b <= x && x < e

transformSeed ls s =
  foldl' (flip transformThroughLayer) s ls

transformInRange s (destStart, sourceStart, rangeLength) =
  if sourceStart <= s && s < sourceStart + rangeLength
    then Just $ s - sourceStart + destStart
    else Nothing

transformThroughLayer ts s =
  case mapMaybe (transformInRange s) ts of
    [] -> s
    [s'] -> s'
    -- stay on the safe side and error on "this should not happen" instead of
    -- getting a non-exhaustive pattern
    _ -> error "Multiple transformations match the same value"

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
