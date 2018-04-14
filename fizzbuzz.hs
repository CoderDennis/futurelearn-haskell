import Data.List

fizzbuzz :: IO ()
fizzbuzz =
  mapM_ putStrLn $ take 100 infinitefizzbuzz

showLine :: (Int, String, String) -> String
showLine (x, "", "") = show x
showLine (_, f, b) = f ++ b

infinitefizzbuzz :: [String]
infinitefizzbuzz = 
  map showLine $ zip3 [1,2..] fizz buzz

fizz :: [String]
fizz = cycle(["","","fizz"])

buzz :: [String]
buzz = cycle(["","","","","buzz"])
