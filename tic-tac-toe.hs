import Data.List

data Square = X | O | Empty

instance Show Square where
  show X = show "X"
  show O = show "O"
  show Empty = show " "

data Grid = List Square deriving (Show)

showGrid :: Show a => [a] -> String
showGrid = intercalate "|" . map show

tic_tac_toe = do
  let grid = [Empty, Empty, Empty,
          Empty, Empty, Empty,
          X, O, X]
  putStrLn (showGrid grid)
