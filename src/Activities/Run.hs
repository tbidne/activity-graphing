module Activities.Run where

import Data.Time (Day)

data Time = MkTime {
  hours :: Integer,
  minutes :: Integer,
  seconds :: Integer
} deriving (Eq, Show)

data Run = MkRun {
  date :: Day,
  distance :: Float,
  time :: Time
} deriving (Eq, Show)

instance Ord Run where
  a <= b = date a <= date b