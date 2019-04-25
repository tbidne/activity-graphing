{-# LANGUAGE RecordWildCards #-}

module Activities.BenchPressList where

import Activities.BenchPress
import Activities.Weights
import Graphable

newtype BenchPressList = MkBPList { unList :: [BenchPress] } deriving Show

instance Graph BenchPressList where
  graph MkBPList{..} = graphVolume date sets unList "Benchpress Volume"