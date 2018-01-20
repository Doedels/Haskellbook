module CasePractice where
-- 1
functionC x y = case (x > y) of
  True  -> x
  False -> y
-- 2
ifEvenAdd2 n = case even n of
  True  -> n + 2
  False -> n
-- 3
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo =(flip dodgy) 2

pal xs
  | xs == reverse xs = True
  | otherwise        = False

numbers x
  | x < 0  = -1
  | x == 0 = 0
  | x > 0  = 1
