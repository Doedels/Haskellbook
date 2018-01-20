module Recursion where
-- testing for incTimes 123456789 0
incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n     = n
incTimes times n = 1 + incTimes1 (times - 1) n
-- 'normal' recursive, gives a stack overflof
incTimes1 :: (Eq a, Num a) => a -> a -> a
incTimes1 0 n     = n
incTimes1 times n = incTimes1 (times - 1) (n + 1)
-- tail recursive, still gives a stack overflow
incTimes2 :: (Eq a, Num a) => a -> a -> a
incTimes2 0 n     = n
incTimes2 times n = incTimes2 (times - 1) $! (n + 1)
-- tail recursive with strict (non-lazy) evaluation ($!) on the accumulating value
-- this prevents a stack overflow!
-- ***************************************************
fib :: (Num t, Eq t) => t -> t
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fac :: (Num t, Eq t) => t -> t
fac 0 = 1
fac n = n * fac (n - 1)
-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
-- Recursion
-- 2
sums :: (Num t, Eq t) => t -> t
sums 0 = 0
sums n = n + sums (n - 1)
-- 3
multi :: (Integral a) => a -> a -> a
multi _ 0 = 0
multi n m = n + multi n (m - 1)
-- Fixing dividedBy
data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> (DividedResult, DividedResult)
dividedBy num denom = if denom == 0
  then (DividedByZero, DividedByZero) else go (abs num) (abs denom) 0
    where go n d count
           | n < d = if ((num < 0) && (denom > 0)) || ((num > 0) && (denom < 0))
             then (Result (negate count), Result (negate n)) else (Result count, Result n)
           | otherwise = go (n - d) d (count + 1)
-- betere oplossing voor dividedBy ?
dividedBy' :: (Num a, Ord a) => a -> a -> DividedResult
dividedBy' _ 0 = DividedByZero
dividedBy' num denom = go num denom 0
  where go n d count
          | n < d = undefined

-- McCarthy 91 function
mc91 :: (Num t, Ord t) => t -> t
mc91 x
  | x > 100 = x - 10
  | otherwise = mc91(mc91 (x + 11))
