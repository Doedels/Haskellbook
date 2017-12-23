module Exercises where

import           Data.Char
-- 2
onlyUpper :: String -> String
onlyUpper = filter isUpper
-- 3
capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs
--- 4
capitalize1 :: String -> String
capitalize1 []     = []
capitalize1 (x:xs) = toUpper x : capitalize1 xs
-- 5
capitalize2 :: [Char] -> Char
capitalize2 xs = toUpper (head xs)
-- 6
capitalize3 :: [Char] -> Char
capitalize3 xs = toUpper . head $ xs

capitalize4 :: [Char] -> Char
capitalize4 = toUpper . head
