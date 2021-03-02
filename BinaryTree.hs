import LinkedList ( LinkedList(..), inverseFold )

data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | End
    deriving (Show)

newTree :: a -> BinaryTree a
newTree x = Node End x End

depth :: Ord a => BinaryTree a -> Integer
depth End = 0
depth (Node left value right) = (depth left + 1) `max` (depth right + 1)

-- Adding value in order and without duplicates to allow searching and sorting
addToSorted :: Ord a => a -> BinaryTree a -> BinaryTree a
addToSorted x End = newTree x
addToSorted x tree@(Node left y right)
    | x < y = Node (addToSorted x left) y right
    | x > y = Node left y (addToSorted x right)
    | otherwise = Node left y right

llToBST :: Ord a => LinkedList a -> BinaryTree a -> BinaryTree a
llToBST Nil _ = End
llToBST (Head y Nil) _ = newTree y
llToBST xs tree = inverseFold addToSorted tree xs
