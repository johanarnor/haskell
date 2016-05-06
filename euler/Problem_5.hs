five = foldl1 lcm [1..20]

-- LOL
-- five x = intersectingValues x [1..1000000]

-- intersectingValues :: Int -> [Int] -> [Int]
-- intersectingValues x list
  -- | x == 0 = list
  -- | otherwise = intersectingValues (x-1) (list `intersect` [x,2*x..1000000])
