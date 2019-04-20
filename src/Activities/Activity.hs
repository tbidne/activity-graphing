{-# LANGUAGE RecordWildCards #-}

module Activities.Activity
( Activity(..)
, ActivityList(..)
, parseToTypedLists
)
where

import Data.List (sort)

import Graphable
import Activities.BenchPress (BenchPress(..))
import Activities.BenchPressList (BenchPressList(..))
import Activities.Deadlift (Deadlift(..))
import Activities.DeadliftList (DeadliftList(..))
import Activities.Run (Run(..))
import Activities.RunList (RunList(..))

data Activity
  = ActBP BenchPress
  | ActD Deadlift
  | ActR Run
  deriving (Eq, Ord, Show)

data ActivityList
  = ActBPList BenchPressList
  | ActDList DeadliftList
  | ActRList RunList
  deriving Show

instance Graph ActivityList where
  graph (ActBPList a) = graph a
  graph (ActDList a) = graph a
  graph (ActRList a) = graph a

filterBenchPress :: [Activity] -> [BenchPress]
filterBenchPress [] = []
filterBenchPress ((ActBP x) : xs) = x : filterBenchPress xs
filterBenchPress (_:xs) = filterBenchPress xs

filterDeadlift :: [Activity] -> [Deadlift]
filterDeadlift [] = []
filterDeadlift ((ActD x) : xs) = x : filterDeadlift xs
filterDeadlift (_:xs) = filterDeadlift xs

filterRun :: [Activity] -> [Run]
filterRun [] = []
filterRun ((ActR x) : xs) = x : filterRun xs
filterRun (_:xs) = filterRun xs

toBPActList :: [Activity] -> ActivityList
toBPActList = ActBPList . MkBPList . sort . filterBenchPress

toDActList :: [Activity] -> ActivityList
toDActList = ActDList . MkDList . sort . filterDeadlift

toRActList :: [Activity] -> ActivityList
toRActList = ActRList . MkRList . sort . filterRun

allFilters :: [[Activity] -> ActivityList]
allFilters = [toBPActList, toDActList, toRActList]

parseToTypedLists :: [Activity] -> [ActivityList]
parseToTypedLists as = [f as | f <- allFilters]