data LinkedList a = Head a (LinkedList a) | Nil
instance (Show a) => Show (LinkedList a) where
    show Nil = show "[]"
    show (Head y Nil) = show y
    show (Head y rest) = show y ++ ", " ++ show rest

getHead :: LinkedList a -> a
getHead (Head y rest) = y

getTail :: LinkedList a -> LinkedList a
getTail Nil = Nil
getTail (Head y rest) = rest

newList :: a -> LinkedList a
newList x = Head x Nil

prepend :: a -> LinkedList a -> LinkedList a
prepend x xs = Head x xs

append :: a -> LinkedList a -> LinkedList a
append x Nil = newList x
append x (Head y rest) = Head y (append x rest)

len :: LinkedList a -> Integer
len Nil = 0
len (Head y rest) = 1 + len rest

contains :: Eq a => a -> LinkedList a -> Bool
contains _ Nil = False
contains x (Head y rest) = x == y || contains x rest

atIndex :: Integer -> LinkedList a -> Maybe a
atIndex 0 (Head y rest) = Just y
atIndex x Nil = Nothing
atIndex x (Head y rest) = atIndex (x - 1) rest

removeFirst :: Eq a => a -> LinkedList a -> LinkedList a
removeFirst x Nil = Nil
removeFirst x (Head y rest)
    | x == y = rest
    | otherwise = Head y (removeFirst x rest)

mapFun :: (a -> b) -> LinkedList a -> LinkedList b
mapFun f Nil = Nil
mapFun f (Head y rest) = Head (f y) (mapFun f rest)

-- 1, 4, 9 -> 9, 4, 1

invert :: LinkedList a -> LinkedList a
invert Nil = Nil
invert (Head y rest) = append y (invert rest)

removeDuplicates :: Eq a => LinkedList a -> LinkedList a
removeDuplicates Nil = Nil
removeDuplicates (Head y rest)
    | contains y rest = removeDuplicates rest
    | otherwise = Head y (removeDuplicates rest)

isAscending :: Ord a => LinkedList a -> Bool
isAscending Nil = True
isAscending (Head _ Nil) = True
isAscending (Head y (Head z rest)) = y >= z && isAscending rest

retain :: Integer -> LinkedList a -> LinkedList a
retain 0 _ = Nil
retain x (Head y rest)
    | x == len (Head y rest) = Head y rest
    | otherwise = Head y (retain (x - 1) rest)

discard :: Integer -> LinkedList a -> LinkedList a
discard 0 xs = xs
discard x (Head _ rest) = discard (x - 1) rest

{-
Support index greater than length of LinkedList

discard :: Integer -> LinkedList a -> LinkedList a
discard _ Nil = Nil
discard x (Head y rest)
    | x > 0 = discard (x - 1) rest
    | otherwise = Head y rest
-}

fold :: (a -> b -> b) -> b -> LinkedList a -> b
fold _ x Nil = x
fold f x (Head y rest) = f y (fold f x rest)

-- Generate a short list with static values for playing around in the inveractive console
forgeList :: Num a => LinkedList a
forgeList = Head 1 (Head 3 (Head 8 (Head 20 Nil)))
