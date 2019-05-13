{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Parser as P
import qualified Activities.Activity as A
import System.Environment (getArgs)
import Text.Megaparsec.Error as ME (errorBundlePretty)

import Graphable

main :: IO ()
main = parseFileName >>= inHelper

parseFileName :: IO (Maybe String)
parseFileName = do
  xs <- getArgs
  case xs of
    [s] -> return (Just s)
    _ -> return Nothing

inHelper :: Maybe String -> IO ()
inHelper =
  \case
    Nothing -> putStrLn "Usage: stack exec graphing-exe <filename>"
    Just s -> graphOrDie . P.entry =<< readFile s

graphOrDie :: Either P.ParseErr [A.Activity] -> IO ()
graphOrDie (Left l) = putStrLn $ ME.errorBundlePretty l
graphOrDie (Right xs) = sequence_ $ fmap graph $ A.parseToTypedLists xs