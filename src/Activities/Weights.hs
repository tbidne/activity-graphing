{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}

module Activities.Weights
( Weight(..)
, WeightList(..)
, WeightType(..)
, graph
, Set(..)
) where

import Prelude hiding (max)
import Graphics.Rendering.Chart.Easy hiding (sets)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Data.Time.Calendar (Day)

import Graphable

data Set = MkSet {
  weight :: Integer,
  reps :: Integer
} deriving (Eq, Show)

data WeightType
  = BenchPress
  | CableCrossover
  | CableRow
  | Deadlift
  | Landmine180
  | LateralRaise
  | LegCurl
  | LegPress
  | PallofPress
  | PullUp
  | ShoulderPress
  | Shrug

data Weight (a :: WeightType) = MkWeight {
  date :: Day,
  duration :: Integer,
  sets :: [Set]
} deriving (Eq, Show)

instance Ord (Weight a) where
  b <= c = date b <= date c

newtype WeightList (a :: WeightType) = MkWeightList { unList :: [Weight a] } deriving Show

instance Graph (WeightList 'BenchPress) where
  graph MkWeightList{..} = graphVolume date sets unList "BenchPress Volume"

instance Graph (WeightList 'CableCrossover) where
  graph MkWeightList{..} = graphVolume date sets unList "Cable Crossover Volume"

instance Graph (WeightList 'CableRow) where
  graph MkWeightList{..} = graphVolume date sets unList "Cable Row Volume"

instance Graph (WeightList 'Deadlift) where
  graph MkWeightList{..} = graphMax date sets unList "Deadlift Max"

instance Graph (WeightList 'Landmine180) where
  graph MkWeightList{..} = graphVolume date sets unList "Landmine 180 Volume"

instance Graph (WeightList 'LateralRaise) where
  graph MkWeightList{..} = graphVolume date sets unList "Lateral Raise Volume"

instance Graph (WeightList 'LegCurl) where
  graph MkWeightList{..} = graphVolume date sets unList "Leg Curl Volume"

instance Graph (WeightList 'LegPress) where
  graph MkWeightList{..} = graphVolume date sets unList "Leg Press Volume"

instance Graph (WeightList 'PallofPress) where
  graph MkWeightList{..} = graphVolume date sets unList "Pallof Press Volume"

instance Graph (WeightList 'PullUp) where
  graph MkWeightList{..} = graphTotalReps date sets unList "Pull Up Volume"

instance Graph (WeightList 'ShoulderPress) where
  graph MkWeightList{..} = graphVolume date sets unList "Shoulder Press Volume"

instance Graph (WeightList 'Shrug) where
  graph MkWeightList{..} = graphMax date sets unList "Shrug Volume"

graphVolume :: (a -> Day) -> (a -> [Set]) -> [a] -> String -> IO ()
graphVolume = graphHelper volume

graphMax :: (a -> Day) -> (a -> [Set]) -> [a] -> String -> IO ()
graphMax = graphHelper max

graphTotalReps :: (a -> Day) -> (a -> [Set]) -> [a] -> String -> IO ()
graphTotalReps = graphHelper totalReps

graphHelper
  :: ((a -> Day) -> (a -> [Set]) -> a -> (Day, Integer))
  -> (a -> Day)
  -> (a -> [Set])
  -> [a]
  -> String
  -> IO ()
graphHelper metric dateFn setsFn act title = toFile def ( "data/" ++ title ++ ".png") $ do
  layout_title .= title
  setColors [opaque blue, opaque red]
  plot (line "Kilograms" [(fmap (metric dateFn setsFn) act)])
  plot (points "Days" (fmap (metric dateFn setsFn) act))

volume :: (a -> Day) -> (a -> [Set]) -> a -> (Day, Integer)
volume dateFn setsFn a = (dateFn a, sumSets a)
  where sumSets = sum . fmap (\MkSet{..} -> weight * reps) . setsFn

max :: (a -> Day) -> (a -> [Set]) -> a -> (Day, Integer)
max dateFn setsFn a = (dateFn a, max' a)
  where max' = maximum . fmap (\MkSet{..} -> weight) . setsFn

totalReps :: (a -> Day) -> (a -> [Set]) -> a -> (Day, Integer)
totalReps dateFn setsFn a = (dateFn a, sumReps a)
  where sumReps = sum . fmap (\MkSet{..} -> reps) . setsFn