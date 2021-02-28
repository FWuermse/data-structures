import LinkedList

data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | End
    deriving Show

newTree :: a -> BinaryTree a
newTree x = Node End x End

-- Adding value in order and without duplicates to allow searching and sorting
addToSorted :: Ord a => a -> BinaryTree a -> BinaryTree a
addToSorted x End = newTree x
addToSorted x tree@(Node left value right)
    | x < value = Node (addToSorted x left) value right
    | x > value = Node left value (addToSorted x right)
    | otherwise = Node left value right

llToBST :: Ord a => LinkedList a -> BinaryTree a -> BinaryTree a
llToBST Nil _ = End
llToBST (Head y Nil) _ = newTree y
llToBST xs tree = fold addToSorted tree xs
