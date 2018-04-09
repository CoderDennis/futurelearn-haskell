-- run tic_tac_toe function to play

import Data.List

tic_tac_toe :: IO ()
tic_tac_toe = 
  turn newGame

data Square 
  = X 
  | O 
  | Empty Char 
  deriving (Eq, Show)

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

showGrid :: Grid -> IO ()
showGrid grid =
  mapM_ putStrLn $ showGridLines grid

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = (take n l) : (chunksOf n $ drop n l)
  | otherwise = error "n must be greater than zero"

showGridLines :: Grid -> [String]
showGridLines grid = 
  intersperse "---------" (map showLine $ chunksOf 3 grid)

showLine :: [Square] -> String
showLine = intercalate " | " . map showSquare

turn :: Game -> IO ()
turn game@(Game {grid = grid, player = player}) = 
  do putStrLn ""
     showGrid grid
     putStrLn ""
     case (findWinner grid) of
       Just winner -> do putStrLn ((show winner) ++ " won!")
                         return ()
       Nothing -> do putStr ((show player) ++ ", enter a square number (q to quit): ")
                     l <- getLine
                     case l of
                       ['q'] -> return ()
                       [s]   -> let sq = Empty s
                                in if sq `elem` grid
                                      then turn Game { grid = replaceSquare grid sq (squareForPlayer player)
                                                     , player = togglePlayer player
                                                     }
                                      else turn game
                       _     -> turn game

replaceSquare :: Grid -> Square -> Square -> Grid
replaceSquare grid oldSquare newSquare =
  let (x,_:ys) = break ((==) oldSquare) grid
   in x ++ newSquare : ys
      
togglePlayer :: Player -> Player
togglePlayer player =
  if player == PlayerX then PlayerO else PlayerX

findWinner :: Grid -> Maybe Player
findWinner grid =
  -- get rows, columns, and diagonals
  -- filter by those that only have X or O values
  -- if no winner and no Empty squares, then Cat is winner
  case (filter isThreeInARow $ getAllLines grid) of
    [[X,X,X]] -> Just PlayerX
    [[O,O,O]] -> Just PlayerO
    _         -> if (any squareIsEmpty grid)
                    then Nothing
                    else Just Cat

getAllLines :: Grid -> [[Square]]
getAllLines grid =
  let diag1 = [grid!!0, grid!!4, grid!!8]
      diag2 = [grid!!2, grid!!4, grid!!6]
      rows = chunksOf 3 grid
      cols = transpose rows
   in [diag1, diag2] ++ rows ++ cols

isThreeInARow :: [Square] -> Bool
isThreeInARow line@[a,_,_] = (nub line) == [a]
isThreeInARow _ = False

squareIsEmpty :: Square -> Bool
squareIsEmpty (Empty _) = True
squareIsEmpty _ = False

