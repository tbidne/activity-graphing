{-# LANGUAGE RecordWildCards #-}

module Activities.BenchPressList where

import Activities.BenchPress
import Activities.Weights
import Graphable

newtype BenchPressList = MkBenchPressList { unList :: [BenchPress] } deriving Show

instance Graph BenchPressList where
  graph MkBenchPressList{..} = graphVolume date sets unList "Benchpress Volume"