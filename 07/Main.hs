module Main where

import Data.Functor (($>))
import Data.List
import Data.Ord
import Data.Void
import Debug.Trace
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, eol)

data Card
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
  deriving (Eq, Show)

newtype Hand = Hand {getCards :: [Card]}
  deriving (Eq, Show)

data HandType
  = FiveOfAKind
  | FourOfAKind
  | FullHouse
  | ThreeOfAKind
  | TwoPair
  | OnePair
  | HighCard
  deriving (Eq, Show)

type Input = Parsec Void String

cardCounts :: Hand -> [(Card, Int)]
cardCounts (Hand cs) =
  concatMap cards . group $ sort cs
  where
    cards cs@(c : _) = [(c, length cs)]

handType hand =
  case sortOn Down . map snd $ cardCounts hand of
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    (3 : _) -> ThreeOfAKind
    (2 : 2 : _) -> TwoPair
    (2 : _) -> OnePair
    _ -> HighCard

handWinnings :: [(Hand, Int)] -> [(Hand, Int)]
handWinnings game =
  zipWith rankWinnings [1 ..] $ sortOn fst game
  where
    rankWinnings rank (hand, bid) = (hand, bid * rank)

winnings = sum . map snd . handWinnings

main :: IO ()
main = do
  stdin <- getContents
  game <- case parse gameP "stdin" stdin of
    Left e -> fail . errorBundlePretty $ e
    Right hs -> pure hs

  print $ winnings game

-- gameP :: Input [(Hand, Int)]
gameP = do
  game <- line `sepEndBy` eol
  eof
  pure game
  where
    line = do
      hand <- handP
      char ' '
      bid <- bidP
      pure (hand, bid)

handP :: Input Hand
handP = do
  c1 <- cardP
  c2 <- cardP
  c3 <- cardP
  c4 <- cardP
  c5 <- cardP
  pure $ Hand [c1, c2, c3, c4, c5]

bidP :: Input Int
bidP = read <$> some digitChar

cardP :: Input Card
cardP =
  choice
    [ char '2' $> Two,
      char '3' $> Three,
      char '4' $> Four,
      char '5' $> Five,
      char '6' $> Six,
      char '7' $> Seven,
      char '8' $> Eight,
      char '9' $> Nine,
      char 'T' $> Ten,
      char 'J' $> Jack,
      char 'Q' $> Queen,
      char 'K' $> King,
      char 'A' $> Ace
    ]

instance Ord Card where
  compare c1 c2 = compare (n c1) (n c2)
    where
      n Ace = 14
      n King = 13
      n Queen = 12
      n Jack = 11
      n Ten = 10
      n Nine = 9
      n Eight = 8
      n Seven = 7
      n Six = 6
      n Five = 5
      n Four = 4
      n Three = 3
      n Two = 2

instance Ord HandType where
  compare ht1 ht2 = compare (n ht1) (n ht2)
    where
      n FiveOfAKind = 7
      n FourOfAKind = 6
      n FullHouse = 5
      n ThreeOfAKind = 4
      n TwoPair = 3
      n OnePair = 2
      n HighCard = 1

instance Ord Hand where
  h1 <= h2
    | handType h1 < handType h2 = True
    | handType h1 == handType h2 = getCards h1 <= getCards h2
    | otherwise = False
