module Main where
import Control.Monad (replicateM)
import qualified Data.HashMap.Strict as Map

data Trie = Trie (Map.HashMap Char Trie)
  deriving (Show)

emptyTrie :: Trie
emptyTrie = Trie Map.empty

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
isListConsistent nums
  | all (isUnique populatedTrie) nums = "YES"
  | otherwise = "NO"
    where populatedTrie = foldr insert emptyTrie nums

insert :: String -> Trie -> Trie
insert [] x = x
insert (c:cs) (Trie children) = case Map.lookup c children of
  Just childTrie -> Trie (Map.insert c (insert cs childTrie) children)
  Nothing -> Trie (Map.insert c (insert cs emptyTrie) children)

isUnique :: Trie -> String  -> Bool
isUnique (Trie children) [] = Map.null children
isUnique (Trie children) (c:cs) = case Map.lookup c children of
  Just childTrie -> isUnique childTrie cs
  Nothing -> True
