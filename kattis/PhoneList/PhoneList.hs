module Main where
import Control.Monad (replicateM)
import Data.List (isPrefixOf, delete)
-- import qualified Data.HashMap.Strict as Map
import qualified Data.Map.Strict as Map

-- data Trie = Trie  { value :: Char
--                   , children :: (Map.Map Char Trie)
--                   }

data Trie = Trie (Map.Map Char Trie)
  deriving (Show)

emptyTrie :: Trie
emptyTrie = Trie (Map.empty)

main :: IO ()
main = do
  noTests <- readLn
  tests <- replicateM noTests readTest
  print tests
  -- putStr (unlines (map consistentList tests))

readTest :: IO [String]
readTest = do
  noNumbers <- readLn
  numbers <- replicateM noNumbers getLine
  return numbers

isListConsistent :: [String] -> Bool
isListConsistent [] = True
isListConsistent (x:xs)
  -- | contains x = False
  | otherwise = isListConsistent xs

insert :: [Char] -> Trie -> Trie
insert [] x = x
insert (c:cs) (Trie children) =
  Trie (Map.insert c (insert cs emptyTrie) children)

contains :: [Char] -> Trie -> Bool
contains [] trie = True
contains (c:cs) (Trie children) = case (Map.lookup c children) of
  Just childTrie -> contains cs childTrie
  Nothing -> False

-- Old Stuff
-- consistentList :: [String] -> String
-- consistentList numbers
--   | consistent numbers = "YES"
--   | otherwise = "NO"
--
-- consistent :: [String] -> Bool
-- consistent [] = True
-- consistent (x:xs)
--   | null $ filter (oneIsPrefixOf x) xs = consistent xs
--   | otherwise = False
--
-- oneIsPrefixOf :: String -> String -> Bool
-- oneIsPrefixOf num1 num2 = num1 `isPrefixOf` num2 || num2 `isPrefixOf` num1
--
-- subNumber :: String -> String -> Bool
-- subNumber num1 num2
--   | length num1 >= length num2 = False
--   | otherwise = num1 == take (length num1) num2
