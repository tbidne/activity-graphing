{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)
import Text.Megaparsec.Error as ME (errorBundlePretty)
import Control.Monad.Trans.Writer.Lazy
import Parser

import Activities.Activity
import Graphable
import App

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
graphOrDie (Right xs) = listsToIO xs
  where actToIO = printLogs . runWriterT . runApp . graph
        listsToIO = sequence_ . fmap actToIO . parseToTypedLists

printLogs :: IO ((), String) -> IO ()
printLogs s = putStrLn . snd =<< s