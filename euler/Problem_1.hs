one = sum (multiples35 [1..999])
  where multiples35 xs = filter multiple35 xs
        multiple35 x
          | x `mod` 3 == 0 = True
          | x `mod` 5 == 0 = True
          | otherwise = False
