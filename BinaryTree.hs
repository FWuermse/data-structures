import LinkedList

data BinaryTree a = Node (BinaryTree a) a (BinaryTree a) | Tip
    deriving (Show)

newTree :: a -> BinaryTree a
newTree x = Node Tip x Tip

depth :: Ord a => BinaryTree a -> Integer
depth Tip = 0
depth (Node left value right) = (depth left + 1) `max` (depth right + 1)

-- Adding value in order and without duplicates to allow searching and sorting
addToSorted :: Ord a => a -> BinaryTree a -> BinaryTree a
addToSorted x Tip = newTree x
addToSorted x tree@(Node left y right)
    | x < y = Node (addToSorted x left) y right
    | x > y = Node left y (addToSorted x right)
    | otherwise = Node left y right

llToBST :: Ord a => LinkedList a -> BinaryTree a -> BinaryTree a
llToBST Nil _ = Tip
llToBST (Head y Nil) _ = newTree y
llToBST xs tree = inverseFold addToSorted tree xs

invertBT :: BinaryTree a -> BinaryTree a
invertBT Tip = Tip
invertBT (Node left y right) = Node (invertBT right) y (invertBT left)
