{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Activities.Activity
( Activity(..)
, ActivityList(..)
, parseToTypedLists
)
where

import Data.List (sort)

import Graphable
import Activities.Weights
import Activities.Run

data Activity where
  ActBenchPress :: Weight 'BenchPress -> Activity
  ActDeadlift :: Weight 'Deadlift -> Activity
  ActRun :: Run -> Activity

data ActivityList where
  ActBenchPressList :: WeightList 'BenchPress -> ActivityList
  ActDeadliftList :: WeightList 'Deadlift -> ActivityList
  ActRunList :: RunList -> ActivityList

instance Graph ActivityList where
  graph (ActBenchPressList a) = graph a
  graph (ActDeadliftList a) = graph a
  graph (ActRunList a) = graph a

filterBenchPress :: [Activity] -> [Weight 'BenchPress]
filterBenchPress = foldr f []
  where f (ActBenchPress x) xs = x : xs
        f _ xs = xs

filterDeadlift :: [Activity] -> [Weight 'Deadlift]
filterDeadlift = foldr f []
  where f (ActDeadlift x) xs = x : xs
        f _ xs = xs

filterRun :: [Activity] -> [Run]
filterRun = foldr f []
  where f (ActRun x) xs = x : xs
        f _ xs = xs

sortedFilter :: Ord b => ([b] -> ActivityList) -> ([Activity] -> [b]) -> [Activity] -> ActivityList
sortedFilter toActList filter' = toActList . sort . filter'

toWeightList :: ([Activity] -> [Weight a]) -> (WeightList a -> ActivityList) -> [Activity] -> ActivityList
toWeightList filter' weightCons = sortedFilter (weightCons . MkWeightList) filter'

toBenchPressActList :: [Activity] -> ActivityList
toBenchPressActList = toWeightList filterBenchPress ActBenchPressList

toDeadliftActList :: [Activity] -> ActivityList
toDeadliftActList = toWeightList filterDeadlift ActDeadliftList

toRunActList :: [Activity] -> ActivityList
toRunActList = sortedFilter (ActRunList . MkRunList) filterRun

allFilters :: [[Activity] -> ActivityList]
allFilters =
  [ toBenchPressActList
  , toDeadliftActList
  , toRunActList ]

parseToTypedLists :: [Activity] -> [ActivityList]
parseToTypedLists as = [f as | f <- allFilters]