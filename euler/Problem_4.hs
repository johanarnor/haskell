four = maximum $ filter palindrome [x | y <- [100..999], z <- [y..999], let x=y*z]

palindrome :: Integer -> Bool
palindrome x = x == reverseInt x

reverseInt :: Integer -> Integer
reverseInt = read . reverse . show
