-- run tic_tac_toe function to play

import Data.List

tic_tac_toe :: IO ()
tic_tac_toe = do
  turn newGame

data Square 
  = X 
  | O 
  | Empty Char 
  deriving (Eq)

data Player
  = PlayerX
  | PlayerO
  | Cat
  deriving (Eq, Show)

type Grid = [Square]

data Game = Game { grid :: Grid
                 , player :: Player
                 }

newGame = 
  Game { grid = [Empty i | i <- ['1'..'9']]
       , player = PlayerX
       }

squareForPlayer :: Player -> Square
squareForPlayer PlayerX = X
squareForPlayer PlayerO = O
squareForPlayer _ = error "Only PlayerX or PlayerO may play a square"

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

turn :: Game -> IO ()
turn game@(Game {grid = grid, player = player}) = 
  do putStrLn ""
     showGrid grid
     putStrLn ""
     putStr ((show player) ++ ", enter a square number (q to quit): ")
     l <- getLine
     let s = (l!!0)
     if s=='q'
        then return ()
        else if (Empty s) `elem` grid
            then let game' = Game { grid = setSquare grid (squareForPlayer player) s
                                  , player = togglePlayer player
                                  }
                   in turn game'
            else turn game

setSquare :: [Square] -> Square -> Char -> [Square]
setSquare grid squareValue squareChar =
  let (x,_:ys) = break (\sq -> matchSquare sq squareChar) grid
   in x ++ squareValue : ys
      
matchSquare :: Square -> Char -> Bool
matchSquare (Empty c) c' = c == c'
matchSquare _ _ = False

togglePlayer :: Player -> Player
togglePlayer player =
  if player == PlayerX then PlayerO else PlayerX

