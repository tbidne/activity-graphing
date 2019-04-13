module Activities.Run where

import Data.Time

data Run = MkRun {
  date :: Day,
  distance :: Integer,
  time :: Integer
} deriving (Eq, Show)

instance Ord Run where
  a <= b = date a <= date b