module GrabBag where
-- 1
mTh x y z = x * y * z
mTh1 x y = \z -> x * y * z
mTh2 x = \y -> \z -> x * y * z
mTh3 = \x -> \y -> \ z -> x * y * z
-- 2
a = mTh 3
-- 3 a
addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = \n -> n + 1
-- Why not this?
addOneIfOdd2 n = case odd n of
  True  -> n + 1
  False -> n
-- b
