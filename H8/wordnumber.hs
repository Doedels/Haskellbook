module WordNumber where

import           Data.List

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "more then one digit"

digits :: Int -> [Int]
digits n = go n []
  where go x xs
         | x == 0 = xs
         | otherwise = go (x `div` 10) ((x `mod` 10) : xs)

digits' :: Int -> [Int]
digits' 0 = []
digits' n = digits' (n `div` 10) ++ [n `mod` 10]

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord (digits n))
