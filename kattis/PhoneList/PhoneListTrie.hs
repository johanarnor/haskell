module Main where
import Control.Monad (replicateM)
import Data.List (isPrefixOf, delete)
import qualified Data.IntMap.Strict as Map
import Data.Char (digitToInt)

data Trie = Trie (Map.IntMap Trie)
  deriving (Show)

emptyTrie :: Trie
emptyTrie = Trie (Map.empty)

main :: IO ()
main = do
  noTests <- readLn
  tests <- replicateM noTests readTest
  putStr (unlines (map isListConsistent tests))

readTest :: IO [String]
readTest = do
  noNumbers <- readLn
  numbers <- replicateM noNumbers getLine
  return numbers

isListConsistent :: [String] -> String
isListConsistent nums
  | consistentLoop nums populatedTrie = "YES"
  | otherwise = "NO"
    where populatedTrie = foldr insert emptyTrie nums

consistentLoop :: [String] -> Trie -> Bool
consistentLoop [] _ = True
consistentLoop (num:nums) trie = case (isUnique num trie) of
  True -> consistentLoop nums (insert num trie)
  False -> False

insert :: [Char] -> Trie -> Trie
insert [] x = x
insert (c:cs) (Trie children) = case (Map.lookup (digitToInt c) children) of
  Just childTrie -> Trie (Map.insert (digitToInt c) (insert cs childTrie) children)
  Nothing -> Trie (Map.insert (digitToInt c) (insert cs emptyTrie) children)

isUnique :: [Char] -> Trie -> Bool
isUnique [] (Trie children) = Map.null children
isUnique (c:cs) (Trie children) = case (Map.lookup (digitToInt c) children) of
  Just childTrie -> isUnique cs childTrie
  Nothing -> True
