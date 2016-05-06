three = last $ factor 600851475143

factor :: Int -> [Int]
factor 1 = []
factor n
  | factors == [] = [n]
  | otherwise = factors ++ factor (n `div` (head factors))
  where factors = take 1 (filter (\x -> n `mod` x == 0) [2..n-1])
