{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

a :: Num a => a
a = (* 9) 6

b :: Num t => (t, [Char])
b = head [(0, "doge"),(1,"kitteh")]

c :: (Integer, [Char])
c = head [(0:: Integer, "doge"),(1,"kitteh")]

d :: Bool
d = if False then True else False

e :: Int
e = length [1,2,3,4,5]

f :: Bool
f = (length [1,2,3,4,5]) > (length "TACOCAT")
