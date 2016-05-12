module Main where
import Test.QuickCheck

main :: IO ()
main = print $ nextPalinQuick 123456700000000

prop_m :: Integer -> Property
prop_m n = n > 0 ==> nextPalinNaive n == nextPalinQuick n

nextPalinNaive :: Integer -> Integer
nextPalinNaive number =
  head $ dropWhile (not . isPalindrome . show) [number+1..]
  where
    isPalindrome list = reverse list == list

nextPalinQuick :: Integer -> Integer
nextPalinQuick number
  | even $ length strNumber = read $ palinEven strNumber
  | otherwise = read $ palinOdd strNumber
  where
    strNumber = show (number + 1)

palinOdd :: String -> String
palinOdd strNumber = half ++ tail (reverse half)
  where half = halfNumberOdd strNumber

halfNumberOdd :: String -> String
halfNumberOdd strNumber
  | secondHalf > reverse firstHalf = (show . (+1) . read) firstHalf
  | otherwise = firstHalf
  where firstHalf = take ((length strNumber `div` 2) + 1) strNumber
        secondHalf = drop (length strNumber `div` 2) strNumber

palinEven :: String -> String
palinEven strNumber = half ++ reverse half
  where half = halfNumberEven strNumber

halfNumberEven :: String -> String
halfNumberEven strNumber
  | secondHalf > reverse firstHalf = (show . (+1) . read) firstHalf
  | otherwise = firstHalf
  where firstHalf = take (length strNumber `div` 2) strNumber
        secondHalf = drop (length strNumber `div` 2) strNumber
