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
  ActCableCrossover :: Weight 'CableCrossover -> Activity
  ActCableRow :: Weight 'CableRow -> Activity
  ActDeadlift :: Weight 'Deadlift -> Activity
  ActLandmine180 :: Weight 'Landmine180 -> Activity
  ActLateralRaise :: Weight 'LateralRaise -> Activity
  ActLegCurl :: Weight 'LegCurl -> Activity
  ActLegPress :: Weight 'LegPress -> Activity
  ActPallofPress :: Weight 'PallofPress -> Activity
  ActPullUp :: Weight 'PullUp -> Activity
  ActShoulderPress :: Weight 'ShoulderPress -> Activity
  ActShrug :: Weight 'Shrug -> Activity
  ActRun :: Run -> Activity

data ActivityList where
  ActBenchPressList :: WeightList 'BenchPress -> ActivityList
  ActCableCrossoverList :: WeightList 'CableCrossover -> ActivityList
  ActCableRowList :: WeightList 'CableRow -> ActivityList
  ActDeadliftList :: WeightList 'Deadlift -> ActivityList
  ActLandmine180List :: WeightList 'Landmine180 -> ActivityList
  ActLateralRaiseList :: WeightList 'LateralRaise -> ActivityList
  ActLegCurlList :: WeightList 'LegCurl -> ActivityList
  ActLegPressList :: WeightList 'LegPress -> ActivityList
  ActPallofPressList :: WeightList 'PallofPress -> ActivityList
  ActPullUpList :: WeightList 'PullUp -> ActivityList
  ActShoulderPressList :: WeightList 'ShoulderPress -> ActivityList
  ActShrugList :: WeightList 'Shrug -> ActivityList
  ActRunList :: RunList -> ActivityList

instance Graph ActivityList where
  graph (ActBenchPressList a) = graph a
  graph (ActCableCrossoverList a) = graph a
  graph (ActCableRowList a) = graph a
  graph (ActDeadliftList a) = graph a
  graph (ActLandmine180List a) = graph a
  graph (ActLateralRaiseList a) = graph a
  graph (ActLegCurlList a) = graph a
  graph (ActLegPressList a) = graph a
  graph (ActPallofPressList a) = graph a
  graph (ActPullUpList a) = graph a
  graph (ActShoulderPressList a) = graph a
  graph (ActShrugList a) = graph a
  graph (ActRunList a) = graph a

fBenchPress :: [Activity] -> [Weight 'BenchPress]
fBenchPress = foldr f []
  where f (ActBenchPress x) xs = x : xs
        f _ xs = xs

fCableCrossover :: [Activity] -> [Weight 'CableCrossover]
fCableCrossover = foldr f []
  where f (ActCableCrossover x) xs = x : xs
        f _ xs = xs

fCableRow :: [Activity] -> [Weight 'CableRow]
fCableRow = foldr f []
  where f (ActCableRow x) xs = x : xs
        f _ xs = xs

fDeadlift :: [Activity] -> [Weight 'Deadlift]
fDeadlift = foldr f []
  where f (ActDeadlift x) xs = x : xs
        f _ xs = xs

fLandmine180 :: [Activity] -> [Weight 'Landmine180]
fLandmine180 = foldr f []
  where f (ActLandmine180 x) xs = x : xs
        f _ xs = xs

fLateralRaise :: [Activity] -> [Weight 'LateralRaise]
fLateralRaise = foldr f []
  where f (ActLateralRaise x) xs = x : xs
        f _ xs = xs

fLegCurl :: [Activity] -> [Weight 'LegCurl]
fLegCurl = foldr f []
  where f (ActLegCurl x) xs = x : xs
        f _ xs = xs

fLegPress :: [Activity] -> [Weight 'LegPress]
fLegPress = foldr f []
  where f (ActLegPress x) xs = x : xs
        f _ xs = xs

fPallofPress :: [Activity] -> [Weight 'PallofPress]
fPallofPress = foldr f []
  where f (ActPallofPress x) xs = x : xs
        f _ xs = xs

fPullUp :: [Activity] -> [Weight 'PullUp]
fPullUp = foldr f []
  where f (ActPullUp x) xs = x : xs
        f _ xs = xs

fShoulderPress :: [Activity] -> [Weight 'ShoulderPress]
fShoulderPress = foldr f []
  where f (ActShoulderPress x) xs = x : xs
        f _ xs = xs

fShrug :: [Activity] -> [Weight 'Shrug]
fShrug = foldr f []
  where f (ActShrug x) xs = x : xs
        f _ xs = xs

fRun :: [Activity] -> [Run]
fRun = foldr f []
  where f (ActRun x) xs = x : xs
        f _ xs = xs

sortedFilter :: Ord b => ([b] -> ActivityList) -> ([Activity] -> [b]) -> [Activity] -> ActivityList
sortedFilter toActList filter' = toActList . sort . filter'

toWeightActList :: ([Activity] -> [Weight a]) -> (WeightList a -> ActivityList) -> [Activity] -> ActivityList
toWeightActList filter' weightCons = sortedFilter (weightCons . MkWeightList) filter'

toBenchPressActList :: [Activity] -> ActivityList
toBenchPressActList = toWeightActList fBenchPress ActBenchPressList

toCableCrossoverActList :: [Activity] -> ActivityList
toCableCrossoverActList = toWeightActList fCableCrossover ActCableCrossoverList

toCableRowActList :: [Activity] -> ActivityList
toCableRowActList = toWeightActList fCableRow ActCableRowList

toDeadliftActList :: [Activity] -> ActivityList
toDeadliftActList = toWeightActList fDeadlift ActDeadliftList

toLandmine180ActList :: [Activity] -> ActivityList
toLandmine180ActList = toWeightActList fLandmine180 ActLandmine180List

toLateralRaiseActList :: [Activity] -> ActivityList
toLateralRaiseActList = toWeightActList fLateralRaise ActLateralRaiseList

toLegCurlActList :: [Activity] -> ActivityList
toLegCurlActList = toWeightActList fLegCurl ActLegCurlList

toLegPressActList :: [Activity] -> ActivityList
toLegPressActList = toWeightActList fLegPress ActLegPressList

toPallofPressActList :: [Activity] -> ActivityList
toPallofPressActList = toWeightActList fPallofPress ActPallofPressList

toPullUpActList :: [Activity] -> ActivityList
toPullUpActList = toWeightActList fPullUp ActPullUpList

toShoulderPressActList :: [Activity] -> ActivityList
toShoulderPressActList = toWeightActList fShoulderPress ActShoulderPressList

toShrugActList :: [Activity] -> ActivityList
toShrugActList = toWeightActList fShrug ActShrugList

toRunActList :: [Activity] -> ActivityList
toRunActList = sortedFilter (ActRunList . MkRunList) fRun

allFilters :: [[Activity] -> ActivityList]
allFilters =
  [ toBenchPressActList
  , toCableCrossoverActList
  , toCableRowActList
  , toDeadliftActList
  , toLandmine180ActList
  , toLateralRaiseActList
  , toLegCurlActList
  , toLegPressActList
  , toPallofPressActList
  , toPullUpActList
  , toShoulderPressActList
  , toShrugActList
  , toRunActList ]

parseToTypedLists :: [Activity] -> [ActivityList]
parseToTypedLists as = [f as | f <- allFilters]