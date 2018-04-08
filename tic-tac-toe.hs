import Data.List
import Data.List.Split

data Square = X | O | Empty Int

showSquare :: Square -> String
showSquare X = "X"
showSquare O = "O"
showSquare (Empty i) = show i

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
  let grid = [Empty i | i <- [1..9]]
  showGrid grid
