module Cipher where

import           Data.Char
import           Data.List

normal :: [Char]
normal = ['a'..'z']

cipher :: (Ord a1, Num a1) => a1 -> [a] -> [a]
cipher shift xs
     | shift == 0 = xs
     | shift > 0  = cipher (shift - 1) (tail xs ++ [head xs])
     | shift < 0  = cipher (shift + 1) (last xs : init xs)

index :: Eq a => a -> [a] -> Int
index ch xs = case findIndex (== ch) xs of
  Just x  -> x
  Nothing -> -1

ceasar :: (Num t, Ord t) => t -> [Char] -> [Char]
ceasar shift string = map encode string
  where
    code = cipher shift normal
    encode x = if ind == (-1) then ' ' else code !! ind
      where
        ind = index (toLower x) normal

unCeasar :: (Ord t, Num t) => t -> [Char] -> [Char]
unCeasar shift string = ceasar (negate shift) string

-- far better implementation
ceasar' :: Int -> [Char] -> [Char]
ceasar' key text = map (chr .
                       (+ ord 'a') .
                       (`mod` 26) .
                       (+ key) .
                       (subtract (ord 'a')) .
                       ord) text

unCeasar' :: (Ord t, Num t) => t -> [Char] -> [Char]
unCeasar' shift string = ceasar' (negate shift) string
