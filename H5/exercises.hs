module Exercises where

-- Given a type, write the function
-- 1
i :: a -> a
i x = x
-- 2
c :: a -> b -> a
c x _ = x
-- 3
c'' :: b -> a -> b
c'' x _ = x
-- 4
c' :: a -> b -> b
c' _ x = x
-- 5
r :: [a] -> [a]
r []     = []
r [_]    = []
r (_:xs) = xs -- r = safe tail?

r' :: [a] -> [a]
r' = reverse
-- 6
co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB x = bToC (aToB x)
-- 7
a :: (a -> c) -> a -> a
a _ x = x
-- 8
a' :: (a -> b) -> a -> b
a' aToB x = aToB x
-- Type-Kwon-Do
-- 1
f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char
h x = g(f x)
-- 2
data A
data B
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e x = w(q x)
-- 3
data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)
-- 4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xy ywz x = fst (ywz(xy x))
