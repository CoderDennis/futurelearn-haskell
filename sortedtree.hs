import Data.List

data Tree = Leaf | Node Int Tree Tree deriving Show

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node val left right) =
  val + (treeSum left) + (treeSum right)

isSortedTree :: Tree -> Bool
isSortedTree t =
  let l = toList t
      s = sort l
   in l == s

addNewMax :: Tree -> Tree
addNewMax Leaf = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2)

insertValue :: Int -> Tree -> Tree
insertValue val Leaf = Node val Leaf Leaf
insertValue val (Node x t1 t2) =
  if (val < x)
    then Node x (insertValue val t1) t2
    else Node x t1 (insertValue val t2)

toList :: Tree -> [Int]
toList Leaf = []
toList (Node x t1 t2) = (toList t1) ++ (x:(toList t2))


