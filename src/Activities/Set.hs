module Activities.Set where

data Set = MkSet {
  weight :: Integer,
  reps :: Integer
} deriving (Eq, Show)