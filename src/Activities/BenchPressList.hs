{-# LANGUAGE RecordWildCards #-}

module Activities.BenchPressList where

import Graphics.Rendering.Chart.Easy ((.=), def, layout_title, plot, line)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Data.Time.Calendar (Day)

import Activities.BenchPress
import Activities.Set
import Activities.Weights
import Graphable

newtype BenchPressList = MkBPList { unList :: [BenchPress] } deriving Show

instance Graph BenchPressList where
  graph MkBPList{..} = graphVolume date sets unList "Benchpress Volume"