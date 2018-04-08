import Data.List
import Data.List.Split

data Square = X | O | Empty Char deriving (Eq)

showSquare :: Square -> String
showSquare X = "X"
showSquare O = "O"
showSquare (Empty i) = [i]

showGrid :: [Square] -> IO ()
showGrid grid =
  mapM_ putStrLn (showGridLines grid)

showGridLines :: [Square] -> [String]
showGridLines grid = 
  let lines = chunksOf 3 grid
  in 
    intersperse "---------" (map showLine lines)

showLine :: [Square] -> String
showLine = intercalate " | " . map showSquare

tic_tac_toe = do
  let grid = [Empty i | i <- ['a'..'i']]
  turn grid X

turn :: [Square] -> Square -> IO ()
turn grid player = 
  do putStrLn ""
     showGrid grid
     putStrLn ""
     putStr ("Player " ++ (showSquare player) ++ ", enter a square (q to quit): ")
     l <- getLine
     let s = (l!!0)
     if s=='q'
        then return ()
        else if (Empty s) `elem` grid
            then turn (setSquare grid player s) (if player == X then O else X)
            else turn grid player

setSquare grid player squareChar =
  --let (x,_:ys) = splitWhen (matchSquare squareChar) grid
  grid
      
matchSquare :: Square -> Char -> Bool
matchSquare (Empty c) c' = c == c'
matchSquare _ _ = False

