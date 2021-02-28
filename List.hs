data LinkedList a = Head a (LinkedList a) | Nil
instance (Show a) => Show (LinkedList a) where
    show Nil = show "Nil"
    show (Head y Nil) = show y
    show (Head y tail) = show y ++ ", " ++ show tail

first :: LinkedList a -> a
first (Head y tail) = y

rest :: LinkedList a -> LinkedList a
rest Nil = Nil
rest (Head y tail) = tail

newList :: a -> LinkedList a
newList x = Head x Nil

prepend :: a -> LinkedList a -> LinkedList a
prepend x xs = Head x xs

append :: a -> LinkedList a -> LinkedList a
append x Nil = newList x
append x (Head y tail) = Head y (append x tail)

len :: LinkedList a -> Integer
len Nil = 0
len (Head y tail) = 1 + len tail

contains :: Eq a => a -> LinkedList a -> Bool
contains _ Nil = False
contains x (Head y tail) = x == y || contains x tail

atIndex :: Integer -> LinkedList a -> Maybe a
atIndex 0 (Head y tail) = Just y
atIndex x Nil = Nothing
atIndex x (Head y tail) = atIndex (x - 1) tail

removeFirst :: Eq a => a -> LinkedList a -> LinkedList a
removeFirst x Nil = Nil
removeFirst x (Head y tail)
    | x == y = tail
    | otherwise = Head y (removeFirst x tail)

mapFun :: (a -> b) -> LinkedList a -> LinkedList b
mapFun f Nil = Nil
mapFun f (Head y tail) = Head (f y) (mapFun f tail)

-- 1, 4, 9 -> 9, 4, 1

invert :: LinkedList a -> LinkedList a
invert Nil = Nil
invert (Head y tail) = append y (invert tail)

removeDuplicates :: Eq a => LinkedList a -> LinkedList a
removeDuplicates Nil = Nil
removeDuplicates (Head y tail)
    | contains y tail = removeDuplicates tail
    | otherwise = Head y (removeDuplicates tail)

isAscending :: Ord a => LinkedList a -> Bool
isAscending Nil = True
isAscending (Head _ Nil) = True
isAscending (Head y (Head z tail)) = y >= z && isAscending tail

retain :: Integer -> LinkedList a -> LinkedList a
retain 0 _ = Nil
retain x (Head y tail)
    | x == len (Head y tail) = Head y tail
    | otherwise = Head y (retain (x - 1) tail)

discard :: Integer -> LinkedList a -> LinkedList a
discard 0 xs = xs
discard x (Head _ tail) = discard (x - 1) tail

{-
Support index greater than length of LinkedList

discard :: Integer -> LinkedList a -> LinkedList a
discard _ Nil = Nil
discard x (Head y tail)
    | x > 0 = discard (x - 1) tail
    | otherwise = Head y tail
-}

fold :: (a -> b -> b) -> b -> LinkedList a -> b
fold _ x Nil = x
fold f x (Head y tail) = f y (fold f x tail)

mergeSort :: Ord a => LinkedList a -> LinkedList a
mergeSort Nil = Nil
mergeSort (Head y Nil) = Head y Nil
mergeSort xs = merge (mergeSort first) (mergeSort second)
    where first = retain half xs
          second = discard half xs
          half = len xs `div` 2

merge :: Ord a => LinkedList a -> LinkedList a -> LinkedList a
merge xs Nil = xs
merge Nil ys = ys
merge xs ys
    | first xs <= first ys = Head (first xs) (merge (rest xs) ys)
    | otherwise = Head (first ys) (merge xs (rest ys))

-- Generate a short list with static values for playing around in the inveractive console
forgeList :: Num a => LinkedList a
forgeList = Head 4 (Head 3 (Head 9 (Head 1 (Head 19 Nil))))
