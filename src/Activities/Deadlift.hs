module Activities.Deadlift where

import Data.Time
import Activities.Set

data Deadlift = MkDeadlift {
  date :: Day,
  duration :: Integer,
  sets :: [Set]
} deriving (Eq, Show)

instance Ord Deadlift where
  a <= b = date a <= date b