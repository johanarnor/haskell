module Main where
import Control.Monad (replicateM)
import Data.List (isPrefixOf, sort)

main :: IO ()
main = do
  noTests <- readLn
  tests <- replicateM noTests readTest
  putStr (unlines (map (isListConsistent . sort) tests))

readTest :: IO [String]
readTest = do
  noNumbers <- readLn
  numbers <- replicateM noNumbers getLine
  return numbers

isListConsistent :: [String] -> String
isListConsistent (_:[]) = "YES"
isListConsistent (x1:x2:xs)
  | x1 `isPrefixOf` x2 = "NO"
  | otherwise = isListConsistent (x2:xs)
