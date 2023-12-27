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

-- Quadratic inequality-based solution
-- assumes that there are always two solutions based on the nature of the
-- exercise
solve a b c =
  let s1 = ((-b) - sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
      s2 = ((-b) + sqrt (b ^ 2 - 4 * a * c)) / (2 * a)
   in (ceiling (min s1 s2), floor (max s1 s2))

-- Returns the first and last "times to charge the boat" that would lead to
-- winning a race
--
-- The "physics" leads to the following quadratic inequality
-- x is the amount of milliseconds we use for "charging" the boat
-- x*(time - x) > distance
-- -x^2 + time * x - distance > 0
solveRace (Race time distance) =
  solve (-1) (realToFrac time) (-realToFrac distance)

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

  let bigRace = Race 63789468 411127420471035
      (s1, s2) = solveRace bigRace
  -- inclusive interval, need to add 1 solution
  print (s2 - s1 + 1)
