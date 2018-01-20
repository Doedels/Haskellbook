module Typecheck where
import           Data.List

--1
data Person = Person Bool deriving (Show)
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
-- 2
data Mood = Blah | Woot deriving (Eq, Show)
settleDown :: Mood -> Mood
settleDown x = if x == Woot then Blah else x
{- 3 a) Values of type Mood
     b) No instance for (Num Mood) arising from the literal `9'
     c) No instance for (Ord Mood) arising from a use of `>' -}
-- 4
type Subject = String
type Verb = String
type Object = String
data Sentence = Sentence Subject Verb Object deriving (Eq, Show)
s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"
s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"
s3 :: Sentence
s3 = s1 "and poo"
-- What can we do?
data Rocks = Rocks String deriving (Eq, Show, Ord)
data Yeah = Yeah Bool deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)
-- 1
phew :: Papu
phew = Papu (Rocks "chases") (Yeah True)
-- 2
truth :: Papu
truth = Papu (Rocks "chomskydoz") (Yeah True)
-- 3
equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'
-- 4
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
-- Match the types
-- 1
i :: Num a => a
i = 1
-- 2
f :: Float
f = 1.0
-- 3
f1 :: Fractional a => a
f1 = 1.0
-- 4
f2 :: RealFloat a => a
f2 = 1.0
-- 5
freud :: Ord a => a -> a
freud x = x
-- 6
freud' :: Int -> Int
freud' x = x
-- 7
myX :: Int
myX = 1
sigmund :: Int -> Int
sigmund x = myX
-- 8
myX' = 1
sigmund' :: Int -> Int
sigmund' x = myX'
-- 9
jung :: Ord a => [a] -> a
jung xs = head (sort xs)
-- 10
young :: [Char] -> Char
young xs = head (sort xs)
-- 11
mySort :: [Char] -> [Char]
mySort = sort
signifier :: [Char] -> Char
signifier xs = head (mySort xs)
-- Type-Kwon-Do 2
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aTob a b = (aTob a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB int a = (aToB a) + (fromIntegral int)
