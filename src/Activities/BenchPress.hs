module Activities.BenchPress where

import Data.Time
import Activities.Set

data BenchPress = MkBenchPress {
  date :: Day,
  duration :: Integer,
  sets :: [Set]
} deriving (Eq, Show)

instance Ord BenchPress where
  a <= b = date a <= date b