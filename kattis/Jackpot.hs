module Main where
import Control.Monad (replicateM)

main :: IO ()
main = do
  noTests <- readLn
  tests <- replicateM noTests readTest
  putStrLn (unlines (map (convertOutput . jackpotCount) tests))

readTest :: IO [Integer]
readTest = do
  _ <- getLine
  line <- getLine
  return (convert line)

convert :: String -> [Integer]
convert = map read . words

jackpotCount :: [Integer] -> Integer
jackpotCount = foldr1 lcm

convertOutput :: Integer -> String
convertOutput x
  | x > 1000000000 = "More than a billion."
  | otherwise = show x
