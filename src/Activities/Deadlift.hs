{-# LANGUAGE RecordWildCards #-}

module Activities.Deadlift where

import Data.Time (Day)
import Activities.Set
import Activities.Weights
import Graphable

data Deadlift = MkDeadlift {
  date :: Day,
  duration :: Integer,
  sets :: [Set]
} deriving (Eq, Show)

instance Ord Deadlift where
  a <= b = date a <= date b

newtype DeadliftList = MkDeadliftList { unList :: [Deadlift] } deriving Show

instance Graph DeadliftList where
  graph MkDeadliftList{..} = graphMax date sets unList "Deadlift Max"