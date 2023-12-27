module Main where

type Millisecond = Int

type Millimeter = Int

-- Speed in millimeters per milliseconds
type Speed = Int

data Race = Race
  { raceTime :: Millisecond,
    raceRecordDistance :: Millimeter
  }
  deriving (Show)

charge :: Millisecond -> Speed
charge ms = ms

winsRace :: Race -> Millisecond -> Bool
winsRace race ms
  | ms > raceTime race = False
  | otherwise =
      distanceTravelled > raceRecordDistance race
  where
    distanceTravelled = charge ms * (raceTime race - ms)

-- Brute-force solution instead of solving the underlying quadratic inequality
solutions :: Race -> [Millisecond]
solutions race = filter (winsRace race) [1 .. (raceTime race)]

main :: IO ()
main = do
  let races =
        [ Race 63 411,
          Race 78 1274,
          Race 94 2047,
          Race 68 1035
        ]
  let solutionSizes = product . map (length . solutions) $ races
  print solutionSizes
