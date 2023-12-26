module Main where

import Control.Monad (void)
import Data.Char (isAlpha)
import Data.Maybe
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, eol, string)

-- (destination start, source start, range length)
type Transformation = (Int, Int, Int)

type Input = Parsec Void String

transformInRange v (destStart, sourceStart, rangeLength) =
  if sourceStart <= v && v < sourceStart + rangeLength
    then Just $ v - sourceStart + destStart
    else Nothing

-- transform :: [Transformation] -> Int -> Int
transformThroughLayer ts v =
  case mapMaybe (transformInRange v) ts of
    [] -> v
    [v'] -> v'
    _ -> error "Multiple transformations match the same value"

transform ls v =
  foldl (flip transformThroughLayer) v ls

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

almanacP :: Input ([Int], [[Transformation]])
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
  case parse almanacP "stdin" stdin of
    Left e -> error . errorBundlePretty $ e
    Right (seeds, layers) -> do
      let ls = map (transform layers) seeds
          minL = minimum ls
      print minL
