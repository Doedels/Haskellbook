module MyFunctions where

myAnd :: [Bool] -> Bool
myAnd []     = True
myAnd (x:xs) = x && myAnd xs
-- 1
myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs
-- 2
myAny :: (t -> Bool) -> [t] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs
-- 3
myElem :: Eq t => t -> [t] -> Bool
myElem _ []     = False
myElem a (x:xs) = a == x || myElem a xs
-- 4
myReverse :: [a] -> [a]
myReverse xs = go [] xs
  where go ys []     = ys
        go ys (z:zs) = go (z:ys) zs
-- 5
squish :: [[a]] -> [a]
squish lss = go [] lss
  where go ys []       = ys
        go ys (xs:xss) = go (ys ++ xs) xss
-- 6
squishMap :: (t -> [a]) -> [t] -> [a]
squishMap f xs = go f xs []
  where go _ [] ys     = ys
        go g (z:zs) ys = go g zs (ys ++ g z)
-- 7
squishAgain :: [[a]] -> [a]
squishAgain xss = squishMap (\x -> x) xss
-- pointfree style
squishAgain' :: [[a]] -> [a]
squishAgain' = squishMap id
-- 8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[])   = x
myMaximumBy f (x:y:xs) = if (f x y == GT) then myMaximumBy f (x:xs) else myMaximumBy f (y:xs)
-- 9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[])   = x
myMinimumBy f (x:y:xs) = if (f x y == LT) then myMinimumBy f (x:xs) else myMinimumBy f (y:xs)
-- 10
myMaximum :: Ord a => [a] -> a
myMaximum xs = myMaximumBy compare xs

myMinimum :: Ord a => [a] -> a
myMinimum xs = myMinimumBy compare xs











































-- 5
