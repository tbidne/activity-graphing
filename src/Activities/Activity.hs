module Activities.Activity
( Activity(..)
, ActivityList(..)
, parseToTypedLists
)
where

import Data.List (sort)

import Graphable
import Activities.BenchPress (BenchPress(..), BenchPressList(..))
import Activities.Deadlift (Deadlift(..), DeadliftList(..))
import Activities.Run (Run(..), RunList(..))

data Activity
  = ActBenchPress BenchPress
  | ActDeadlift Deadlift
  | ActRun Run
  deriving (Eq, Ord, Show)

data ActivityList
  = ActBenchPressList BenchPressList
  | ActDeadliftList DeadliftList
  | ActRunList RunList
  deriving Show

instance Graph ActivityList where
  graph (ActBenchPressList a) = graph a
  graph (ActDeadliftList a) = graph a
  graph (ActRunList a) = graph a

filterBenchPress :: [Activity] -> [BenchPress]
filterBenchPress = foldr f []
  where f (ActBenchPress x) xs = x : xs
        f _ xs = xs

filterDeadlift :: [Activity] -> [Deadlift]
filterDeadlift = foldr f []
  where f (ActDeadlift x) xs = x : xs
        f _ xs = xs

filterRun :: [Activity] -> [Run]
filterRun = foldr f []
  where f (ActRun x) xs = x : xs
        f _ xs = xs

sortedFilter :: Ord b => ([b] -> ActivityList) -> ([Activity] -> [b]) -> [Activity] -> ActivityList
sortedFilter toActList filter' = toActList . sort . filter'

toBenchPressActList :: [Activity] -> ActivityList
toBenchPressActList = sortedFilter (ActBenchPressList . MkBenchPressList) filterBenchPress

toDeadliftActList :: [Activity] -> ActivityList
toDeadliftActList = sortedFilter (ActDeadliftList . MkDeadliftList) filterDeadlift

toRunActList :: [Activity] -> ActivityList
toRunActList = sortedFilter (ActRunList . MkRunList) filterRun

allFilters :: [[Activity] -> ActivityList]
allFilters =
  [ toBenchPressActList
  , toDeadliftActList
  , toRunActList ]

parseToTypedLists :: [Activity] -> [ActivityList]
parseToTypedLists as = [f as | f <- allFilters]