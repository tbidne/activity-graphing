{-# LANGUAGE RecordWildCards #-}

module Activities.DeadliftList where

import Graphics.Rendering.Chart.Easy ((.=), def, layout_title, plot, line)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Data.Time.Calendar (Day)

import Activities.Deadlift
import Activities.Set
import Graphable

newtype DeadliftList = MkDList { unList :: [Deadlift] } deriving Show

instance Graph DeadliftList where
    graph MkDList{..} = toFile def "data/Deadlift.png" $ do
      layout_title .= "Deadlift"
      plot (line "" [(fmap volume unList)])

volume :: Deadlift -> (Day, Integer)
volume MkDeadlift{..} = (date, foldr (\s i -> f s + i) 0 sets)
  where f MkSet{..} = weight * reps