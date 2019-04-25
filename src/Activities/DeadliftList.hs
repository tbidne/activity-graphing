{-# LANGUAGE RecordWildCards #-}

module Activities.DeadliftList where

import Graphics.Rendering.Chart.Easy ((.=), def, layout_title, plot, line)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Data.Time.Calendar (Day)

import Activities.Deadlift
import Activities.Set
import Activities.Weights
import Graphable

newtype DeadliftList = MkDList { unList :: [Deadlift] } deriving Show

instance Graph DeadliftList where
  graph MkDList{..} = graphMax date sets unList "Deadlift Max"