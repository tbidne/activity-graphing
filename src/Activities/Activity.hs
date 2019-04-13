{-# LANGUAGE RecordWildCards #-}

module Activities.Activity where

import Data.List (sort)

import Activities.BenchPress (BenchPress(..))
import Activities.Deadlift (Deadlift(..))
import Activities.Run (Run(..))

data Activity = ActBP BenchPress | ActD Deadlift | ActR Run
  deriving (Eq, Ord, Show)

class DataPoint a where
  point :: a -> String

instance DataPoint BenchPress where
  point MkBenchPress{..} = "BP: " ++ show date

instance DataPoint Deadlift where
  point MkDeadlift{..} = "D: " ++ show date

instance DataPoint Run where
  point MkRun{..} = "R: " ++ show date

newtype BPList = MkBPList { bpList :: [BenchPress] } deriving Show
newtype DList = MkDList { dList :: [Deadlift] } deriving Show
newtype RList = MkRList { rList :: [Run] } deriving Show

data ActivityList = ActBPList BPList | ActDList DList | ActRList RList
  deriving Show

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