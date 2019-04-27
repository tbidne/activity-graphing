{-# LANGUAGE RecordWildCards #-}

module Activities.Weights
( graphVolume
, graphMax
)
where

import Prelude hiding (max)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Data.Time.Calendar (Day)

import Activities.Set (Set(..))

graphVolume :: (a -> Day) -> (a -> [Set]) -> [a] -> String -> IO ()
graphVolume = graphHelper volume

graphMax :: (a -> Day) -> (a -> [Set]) -> [a] -> String -> IO ()
graphMax = graphHelper max

graphHelper :: ((a -> Day) -> (a -> [Set]) -> a -> (Day, Integer))
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