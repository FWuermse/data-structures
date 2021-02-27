data Tree a = Node (Tree a) a (Tree a) | Nil
    deriving Show

newTree :: a -> Tree a
newTree x = Node Nil x Nil

addTopRight :: a -> Tree a -> Tree a
addTopRight right tree = Node tree right Nil

addTopLeft :: a -> Tree a -> Tree a
addTopLeft left tree = Node Nil left tree
