import Data.List

fizzbuzz =
  mapM_ putStrLn $ take 100 infinitefizzbuzz

showLine (x, "", "") = show x
showLine (_, f, b) = f ++ b

infinitefizzbuzz = 
  map showLine $ zip3 [1,2..] fizz buzz

fizz = cycle(["","","fizz"])

buzz = cycle(["","","","","buzz"])
