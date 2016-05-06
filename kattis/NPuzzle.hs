module Main where

type Coordinate = (Int, Int)

main :: IO ()
main = do
  line1 <- getLine
  line2 <- getLine
  line3 <- getLine
  line4 <- getLine
  print $ sum $ listDistances (line1 : line2 : line3 : line4 : [])

listDistances :: [[Char]] -> [Int]
listDistances x = outerHelper x 0

outerHelper :: [[Char]] -> Int -> [Int]
outerHelper [] _ = []
outerHelper (x : xs) row =
  (innerHelper x row 0) ++ outerHelper xs (row + 1)

innerHelper :: [Char] -> Int -> Int -> [Int]
innerHelper [] _ _ = []
innerHelper ('.' : xs) row col = (0 : innerHelper xs row (col + 1))
innerHelper (x : xs) row col =
  (distance (defaultPos x) (row, col) : innerHelper xs row (col + 1))

defaultPos :: Char -> Coordinate
defaultPos c = (val `quot` 4, val `mod` 4)
  where val = fromEnum c - fromEnum 'A'

distance :: Coordinate -> Coordinate -> Int
distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
