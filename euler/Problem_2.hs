two = sum (filter even (takeWhile (<4000000) (fibHelp 1 1)))

fibHelp :: Int -> Int -> [Int]
fibHelp x y = x : fibHelp y (x + y)
