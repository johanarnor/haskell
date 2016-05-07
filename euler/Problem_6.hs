module Main where

main :: IO ()
main = print six

six :: Integer
six = (^2) (sum xs) - sum (map (^2) xs)
  where xs = [1..100]
