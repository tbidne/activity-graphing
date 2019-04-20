{-# LANGUAGE RecordWildCards #-}

module Activities.BenchPressList where

import Graphics.Rendering.Chart.Easy ((.=), def, layout_title, plot, line)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Data.Time.Calendar (Day)

import Activities.BenchPress
import Activities.Set
import Graphable

newtype BenchPressList = MkBPList { unList :: [BenchPress] } deriving Show

instance Graph BenchPressList where
    graph MkBPList{..} = toFile def "data/Benchpress.png" $ do
      layout_title .= "BenchPress"
      plot (line "" [(fmap volume unList)])

volume :: BenchPress -> (Day, Integer)
volume MkBenchPress{..} = (date, foldr (\s i -> f s + i) 0 sets)
  where f MkSet{..} = weight * reps