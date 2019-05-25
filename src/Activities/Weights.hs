{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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
  graph MkWeightList{..} = graphVolume unList "BenchPress Volume"

instance Graph (WeightList 'CableCrossover) where
  graph MkWeightList{..} = graphVolume unList "Cable Crossover Volume"

instance Graph (WeightList 'CableRow) where
  graph MkWeightList{..} = graphVolume unList "Cable Row Volume"

instance Graph (WeightList 'Deadlift) where
  graph MkWeightList{..} = graphMax unList "Deadlift Max"

instance Graph (WeightList 'Landmine180) where
  graph MkWeightList{..} = graphVolume unList "Landmine 180 Volume"

instance Graph (WeightList 'LateralRaise) where
  graph MkWeightList{..} = graphVolume unList "Lateral Raise Volume"

instance Graph (WeightList 'LegCurl) where
  graph MkWeightList{..} = graphVolume unList "Leg Curl Volume"

instance Graph (WeightList 'LegPress) where
  graph MkWeightList{..} = graphVolume unList "Leg Press Volume"

instance Graph (WeightList 'PallofPress) where
  graph MkWeightList{..} = graphVolume unList "Pallof Press Volume"

instance Graph (WeightList 'PullUp) where
  graph MkWeightList{..} = graphTotalReps unList "Pull Up Volume"

instance Graph (WeightList 'ShoulderPress) where
  graph MkWeightList{..} = graphVolume unList "Shoulder Press Volume"

instance Graph (WeightList 'Shrug) where
  graph MkWeightList{..} = graphMax unList "Shrug Volume"

graphVolume :: [Weight a] -> String -> IO ()
graphVolume = graphHelper volume

graphMax :: [Weight a] -> String -> IO ()
graphMax = graphHelper max

graphTotalReps :: [Weight a] -> String -> IO ()
graphTotalReps = graphHelper totalReps

graphHelper :: (Weight a -> (Day, Integer)) -> [Weight a] -> String -> IO ()
graphHelper _ [] title = putStrLn $ "no data for " ++ title
graphHelper metric as title = toFile def ( "data/" ++ title ++ ".png") $ do
  layout_title .= title
  setColors [opaque blue, opaque red]
  plot $ line "Kilograms" [fmap metric as]
  plot $ points "Days" $ fmap metric as

volume :: Weight a -> (Day, Integer)
volume a = (date a, sumSets a)
  where sumSets = sum . fmap (\MkSet{..} -> weight * reps) . sets

max :: Weight a -> (Day, Integer)
max a = (date a, max' a)
  where max' = maximum . fmap (\MkSet{..} -> weight) . sets

totalReps :: Weight a -> (Day, Integer)
totalReps a = (date a, sumReps a)
  where sumReps = sum . fmap (\MkSet{..} -> reps) . sets