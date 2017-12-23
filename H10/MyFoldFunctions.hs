module MyFoldFunctions where

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True
-- 1
myOr :: [Bool] -> Bool
myOr = foldr (||) False
-- 2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False
-- 3
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr ((||) . (==) x) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (x ==)
-- 4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []
-- 5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []
-- 6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a then a : b else b) []
-- 7
squish :: [[a]] -> [a]
squish = foldr (++) []
-- 8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []
-- 9
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
-- 10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\a b -> if (f a b == GT) then a else b) x xs
-- 11  Since we use the 1st element of te list we can use foldl1
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f = foldl1 (\a b -> if (f a b == LT) then a else b)
