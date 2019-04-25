{-# LANGUAGE RecordWildCards #-}

module Activities.DeadliftList where

import Activities.Deadlift
import Activities.Weights
import Graphable

newtype DeadliftList = MkDeadliftList { unList :: [Deadlift] } deriving Show

instance Graph DeadliftList where
  graph MkDeadliftList{..} = graphMax date sets unList "Deadlift Max"