module Main where

import qualified Parser as P
import qualified Activities.Activity as A

import Text.Megaparsec.Error as ME (errorBundlePretty)

import Graphable

main :: IO ()
main = do
  str <- readFile "data/sample.txt"
  graphOrDie $ P.entry str

graphOrDie :: Either P.ParseErr [A.Activity] -> IO ()
graphOrDie (Left l) = putStrLn $ ME.errorBundlePretty l
graphOrDie (Right xs) = sequence_ $ fmap graph $ A.parseToTypedLists xs