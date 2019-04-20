module Activities.Run where

import Data.Time (Day)

data Run = MkRun {
  date :: Day,
  distance :: Float,
  time :: Integer
} deriving (Eq, Show)

instance Ord Run where
  a <= b = date a <= date b