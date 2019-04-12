module Main where

import qualified Parser as P

main :: IO ()
main = do
  str <- readFile "data/sample.txt"
  let f = putStrLn . concat . fmap P.test . P.entry
  f str
