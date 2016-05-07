module Main where
import Control.Monad (replicateM)
import Data.List (isPrefixOf)

main :: IO ()
main = do
  noTests <- readLn
  tests <- replicateM noTests readTest
  putStr (unlines (map isListConsistent tests))

readTest :: IO [String]
readTest = do
  noNumbers <- readLn
  replicateM noNumbers getLine

isListConsistent :: [String] -> String
isListConsistent [] = "YES"
isListConsistent (num:nums)
  | not $ any (oneIsPrefixOf num) nums = isListConsistent nums
  | otherwise = "NO"

oneIsPrefixOf :: String -> String -> Bool
oneIsPrefixOf num1 num2 = num1 `isPrefixOf` num2 || num2 `isPrefixOf` num1
