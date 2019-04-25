{-# LANGUAGE RecordWildCards #-}

module Activities.DeadliftList where

import Activities.Deadlift
import Activities.Weights
import Graphable

newtype DeadliftList = MkDList { unList :: [Deadlift] } deriving Show

instance Graph DeadliftList where
  graph MkDList{..} = graphMax date sets unList "Deadlift Max"