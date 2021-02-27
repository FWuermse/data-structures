data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | Nil
    deriving Show

newTree :: a -> BinaryTree a
newTree x = Node Nil x Nil

addTopRight :: a -> BinaryTree a -> BinaryTree a
addTopRight right tree = Node tree right Nil

addTopLeft :: a -> BinaryTree a -> BinaryTree a
addTopLeft left tree = Node Nil left tree
