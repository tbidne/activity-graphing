module Graphable where

class Graph a where
  graph :: a -> IO ()