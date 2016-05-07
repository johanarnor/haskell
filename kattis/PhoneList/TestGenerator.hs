module Main where
import System.Random
import Data.List

main :: IO ()
main = do
  let noTests = 1
  let noNumbers = 50000
  seed <- newStdGen
  let numbers = randomList noNumbers seed
  writeFile "test.txt" (unlines (map show (noTests:noNumbers:numbers)))

randomList :: Int -> StdGen -> [Int]
randomList n = take n . unfoldr (Just . randomR (1, 999999999))
