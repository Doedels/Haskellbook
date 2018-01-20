module Exercises where
-- Lets write code 1
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10
-- or...
tensDigit1 x = (x `div` 10) `mod` 10
-- a with divMod
tensDigit2 x = snd (divMod (fst (divMod x 10)) 10)
-- c
hunsD x = (x `div` 100) `mod` 10
-- 2
foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
  False -> x
  _     -> y

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z
  | z          = y
  | otherwise  = x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y
-- 3
g :: (a -> b) -> (a, c) -> (b, c)
g aToB tuple = case tuple of
  (a, c) -> (aToB a, c)
