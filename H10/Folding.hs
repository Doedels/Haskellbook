module Folding where


xs :: [String]
xs = map show [1..5]

a :: String
a = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" xs

b :: String
b = foldl (\x y -> concat ["(",x,"*",y,")"]) "0" xs

myAny :: Foldable t1 => (t -> Bool) -> t1 t -> Bool
myAny f = foldr (\x y -> f x || y) False

fib :: [Integer]
fib = take 20 (1 : scanl (+) 1 fib)

fac :: [Integer]
fac = take 20 (1 : scanl (*) 2 [3..])
