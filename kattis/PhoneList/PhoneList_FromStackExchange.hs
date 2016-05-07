-- Not mine, from http://codereview.stackexchange.com/a/92540
import Control.Monad (replicateM, replicateM_)
import Data.List (isPrefixOf, sort)

isConsistentPhoneList :: [String] -> Bool
isConsistentPhoneList numbers = not $ any (uncurry isPrefixOf) consecutivePairs
  where
    sortedNumbers = sort numbers
    consecutivePairs = zip sortedNumbers $ tail sortedNumbers

yesNo :: Bool -> String
yesNo True = "YES"
yesNo False = "NO"

processCase :: IO ()
processCase = do
  caseSize <- readLn
  inputs <- replicateM caseSize getLine
  putStrLn $ (yesNo . isConsistentPhoneList) inputs

main :: IO ()
main = do
  caseCount <- readLn
  replicateM_ caseCount processCase
