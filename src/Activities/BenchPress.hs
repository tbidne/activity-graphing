{-# LANGUAGE RecordWildCards #-}

module Activities.BenchPress where

import Data.Time (Day)
import Activities.Set
import Activities.Weights
import Graphable

data BenchPress = MkBenchPress {
  date :: Day,
  duration :: Integer,
  sets :: [Set]
} deriving (Eq, Show)

instance Ord BenchPress where
  a <= b = date a <= date b

newtype BenchPressList = MkBenchPressList { unList :: [BenchPress] } deriving Show

instance Graph BenchPressList where
  graph MkBenchPressList{..} = graphVolume date sets unList "Benchpress Volume"