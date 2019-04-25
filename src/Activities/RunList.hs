{-# LANGUAGE RecordWildCards #-}

module Activities.RunList where

import Graphics.Rendering.Chart.Easy ((.=), def, layout_title, plot, line, PlotValue)
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Data.Time.Calendar (Day)

import Activities.Run
import Graphable

newtype RunList = MkRunList { unList :: [Run] } deriving Show

instance Graph RunList where
  graph r =
    graphHelper r "Run Distance" byDist
    *> graphHelper r "Run Time" byTime

graphHelper :: PlotValue a => RunList -> String -> (Run -> (Day, a)) -> IO ()
graphHelper MkRunList{..} title f = toFile def ( "data/" ++ title ++ ".png") $ do
  layout_title .= title
  plot (line "" [(fmap f unList)])

byDist :: Run -> (Day, Float)
byDist MkRun{..} = (date, distance)

byTime :: Run -> (Day, Integer)
byTime MkRun{..} = (date, time)