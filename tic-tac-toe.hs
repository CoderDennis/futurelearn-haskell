import Data.List
import Data.List.Split

data Square = X | O | Empty

showSquare :: Square -> String
showSquare X = "X"
showSquare O = "O"
showSquare Empty = " "

data Grid = List Square

showGrid :: [Square] -> [String]
showGrid grid = 
  let lines = chunksOf 3 grid
  in 
    map showGridLine lines

showGridLine = intercalate "|" . map showSquare

tic_tac_toe = do
  let grid = [Empty, Empty, Empty,
          Empty, Empty, Empty,
          X, O, X]
  show (showGrid grid)
