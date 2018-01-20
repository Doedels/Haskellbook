module Exercises where

awesome :: [String]
awesome = ["papuchon", "curry", ":)"]

also :: [String]
also = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, also]

-- opdracht 8
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- opdracht 9
myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

-- opdracht 10
f :: (a,b) -- x
  -> (c,d) -- y
  -> ((b,d), (a,c))
-- LET OP de type signature op verschillende regels, zodat je per regel comments kunt toevoegen
f x y = ((snd x, snd y), (fst x, fst y))

-- correcting syntax
-- opracht 1
f1 :: Foldable t => t a -> Int
f1 xs = length xs + 1

-- opdracht 2
f2 :: t -> t
f2 x = x

-- opdracht 3
f3 :: (t, t1) -> t
f3 (a, _) = a
