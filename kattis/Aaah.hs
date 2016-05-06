main :: IO ()
main = do
  patient <- getLine
  doctor <- getLine
  putStrLn $ aaah patient doctor

aaah :: String -> String -> String
aaah patient doctor
  | length patient >= length doctor = "go"
  | otherwise = "no"
