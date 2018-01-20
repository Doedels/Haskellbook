module Enums where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool _ _        = []

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ GT = [EQ, GT]
eftOrd _ _   = []

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
  where go n m xs
         | m < n = xs
         | otherwise = go n (m - 1) (m : xs)

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
  where go n m xs
         | m < n = xs
         | otherwise = go n (pred m) (m : xs)


eftAll :: (Ord a, Bounded a, Enum a) => a -> a -> [a]
eftAll x y = go x y []
  where go n m xs
         | m < n = xs
         | m == minBound = m : xs
         | otherwise = go n (pred m) (m : xs)
