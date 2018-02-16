module LibraryFunctions where

import Data.Monoid
-- implemetnt these functions in terms of foldMap or foldr from Foldable
-- added '1' to the names to distinguish from possible prelude functions
-- 1
sum1 :: (Foldable t, Num a) => t a -> a
sum1 = getSum . foldMap Sum
-- sum1 = foldr (+) 0

product1 :: (Foldable t, Num a) => t a -> a
product1 = getProduct . foldMap Product
-- product1 = foldr (*) 1

elem1 :: (Foldable t, Eq a) => a -> t a -> Bool
elem1 a = foldr ( (||) . (== a) ) False

minimum1 :: (Foldable t, Ord a) => t a -> Maybe a
minimum1 ta = if null1 ta then Nothing else Just (foldr1 min ta)

maximum1 :: (Foldable t, Ord a) => t a -> Maybe a
maximum1 ta = if null1 ta then Nothing else Just (foldr1 max ta)

null1 :: Foldable t => t a -> Bool
null1 = foldr ( (&&) . (\_ -> False)) True

length1 :: Foldable t => t a -> Int
length1 = foldr ( (+) . (\_ -> 1) ) 0

toList1 :: Foldable t => t a -> [a]
toList1 = foldr (:) []
       -- foldMap pure

fold1 :: (Foldable t, Monoid m) => t m -> m
fold1 = foldMap id

foldMap1 :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap1 f = foldr ( (<>) . f ) mempty
