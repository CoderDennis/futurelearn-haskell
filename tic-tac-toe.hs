-- run tic_tac_toe function to play

import Data.List

tic_tac_toe :: IO ()
tic_tac_toe = do
  let grid = [Empty i | i <- ['a'..'i']]
  turn grid X

data Square = X | O | Empty Char deriving (Eq)

showSquare :: Square -> String
showSquare X = "X"
showSquare O = "O"
showSquare (Empty i) = [i]

showGrid :: [Square] -> IO ()
showGrid grid =
  mapM_ putStrLn (showGridLines grid)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n (drop n l))
  | otherwise = error "n must be greater than zero"

showGridLines :: [Square] -> [String]
showGridLines grid = 
  let lines = chunksOf 3 grid
  in 
    intersperse "---------" (map showLine lines)

showLine :: [Square] -> String
showLine = intercalate " | " . map showSquare


turn :: [Square] -> Square -> IO ()
turn grid player = 
  do putStrLn ""
     showGrid grid
     putStrLn ""
     putStr ("Player " ++ (showSquare player) ++ ", enter a square letter (q to quit): ")
     l <- getLine
     let s = (l!!0)
     if s=='q'
        then return ()
        else if (Empty s) `elem` grid
            then turn (setSquare grid player s) (if player == X then O else X)
            else turn grid player

setSquare :: [Square] -> Square -> Char -> [Square]
setSquare grid player squareChar =
  let (x,_:ys) = break (\sq -> matchSquare sq squareChar) grid
   in x ++ player : ys
      
matchSquare :: Square -> Char -> Bool
matchSquare (Empty c) c' = c == c'
matchSquare _ _ = False

