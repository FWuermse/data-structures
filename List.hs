data LinkedList a = Head a (LinkedList a) | Nil
instance (Show a) => Show (LinkedList a) where
    show (Head y Nil) = show y
    show (Head y rest) = show y ++ ", " ++ show rest

newList :: a -> LinkedList a
newList x = Head x Nil

add :: a -> LinkedList a -> LinkedList a
add = Head

append :: a -> LinkedList a -> LinkedList a
append x Nil = newList x
append x (Head y rest) = Head y (append x rest)

len :: LinkedList a -> Integer
len Nil = 0
len (Head y rest) = 1 + len rest

contains :: Eq a => a -> LinkedList a -> Bool
contains x Nil = False
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

forgeList :: Num a => LinkedList a
forgeList = Head 8 (Head 10 (Head 5 (Head 7 Nil)))

mapFun :: (a -> b) -> LinkedList a -> LinkedList b
mapFun f Nil = Nil
mapFun f (Head y rest) = Head (f y) (mapFun f rest)

{-
1, 4, 9 -> 9, 4, 1
-}

invert :: LinkedList a -> LinkedList a
invert Nil = Nil
invert (Head y rest) = append y (invert rest)
