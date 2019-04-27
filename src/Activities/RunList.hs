{-# LANGUAGE RecordWildCards #-}

module Activities.RunList where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo (toFile)
import Data.Time.Calendar (Day)

import Activities.Run
import Graphable

newtype RunList = MkRunList { unList :: [Run] } deriving Show

instance Graph RunList where
  graph r =
    graphHelper r "Run Distance" "Kilometers" byDist
    *> graphHelper r "Run Time" "Minutes" byTime
    *> graphHelper r "Run Pace" "Minutes / Kilometer" byPace

graphHelper :: PlotValue a => RunList -> String -> String -> (Run -> (Day, a)) -> IO ()
graphHelper MkRunList{..} title legend f = toFile def ( "data/" ++ title ++ ".png") $ do
  layout_title .= title
  setColors [opaque blue, opaque red]
  plot (line legend [(fmap f unList)])
  plot (points "Days" (fmap f unList))

byDist :: Run -> (Day, Float)
byDist MkRun{..} = (date, distance)

byTime :: Run -> (Day, Float)
byTime MkRun{..} = (date, toMinutes time)

byPace :: Run -> (Day, Float)
byPace MkRun{..} = (date, (toMinutes time) / distance)

toSeconds :: Time -> Integer
toSeconds MkTime{..} = (hours * 3600) + (minutes * 60) + seconds

toMinutes :: Time -> Float
toMinutes MkTime{..} = h * 60 + m + s / 60.0
  where [h, m, s] = fmap fromIntegral [hours, minutes, seconds]