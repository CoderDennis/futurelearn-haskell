-- run tic_tac_toe function to play

import Data.List

tic_tac_toe :: IO ()
tic_tac_toe = 
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
     case (findWinner game) of
       Just winner -> do putStrLn ((show winner) ++ " won!")
                         return ()
       Nothing -> do putStr ((show player) ++ ", enter a square number (q to quit): ")
                     l <- getLine
                     let s = (l!!0)
                     if s=='q'
                        then return ()
                        else let sq = (Empty s)
                             in if sq `elem` grid
                                   then turn Game { grid = replaceSquare grid sq (squareForPlayer player)
                                                  , player = togglePlayer player
                                                  }
                                   else turn game

replaceSquare :: [Square] -> Square -> Square -> [Square]
replaceSquare grid oldSquare newSquare =
  let (x,_:ys) = break ((==) oldSquare) grid
   in x ++ newSquare : ys
      
togglePlayer :: Player -> Player
togglePlayer player =
  if player == PlayerX then PlayerO else PlayerX

{-
-- With any 3x3 magic square the sum of each row, column, and diagonal is 15.
-- This is an example and may be useful for finding a winner.
magicNumbers = [ 2, 7, 6
               , 9, 5, 1
               , 4, 3, 8
               ]
-}

findWinner :: Game -> Maybe Player
findWinner (Game {grid=grid}) =
  Nothing
  -- zip grid with magicNumbers?
  -- get rows, columns, and diagonals
  -- filter by those that only have X or O values
  -- use magic numbers to find a sum of 15 for any given row/column/diagonal
  -- if no winner and no Empty squares, then Cat is winner

