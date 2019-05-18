{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Text.Megaparsec.Error as ME (errorBundlePretty)

import Parser
import Activities.Activity
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
    Just s -> graphOrDie . entry =<< readFile s

graphOrDie :: Either ParseErr [Activity] -> IO ()
graphOrDie (Left l) = putStrLn $ ME.errorBundlePretty l
graphOrDie (Right xs) = sequence_ $ fmap graph $ parseToTypedLists xs