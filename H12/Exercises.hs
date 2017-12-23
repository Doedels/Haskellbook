module Exercises where

import           Data.List

-- String processing 1
replaceThe :: String -> String
replaceThe s = unwords(foldr replace [] (words s))
  where replace a bs
              | a == "the" = "a" : bs
              | otherwise = a : bs

replaceThe' :: String -> String
replaceThe' s = go (words s) []
  where go [] as = unwords . reverse $ as
        go (b:bs) as
           | b == "the" = go bs ("a" : as)
           | otherwise = go bs (b : as)
-- 2
countTheBeforeVowel :: String -> Int
countTheBeforeVowel s = go (words s) []
  where go [] as = length as
        go [_] as = length as
        go (b:c:cs) as
            | b == "the" && head c `elem` "aeiou" = go cs (b:as)
            | otherwise = go (c:cs) as
-- 3
countVowels :: String -> Int
countVowels s = length (filter (`elem` "aeiou") s)
-- validate the word
newtype Word' = Word' String deriving (Show, Eq)

mkWord :: String -> Maybe Word'
mkWord s
     | length s - vowels >= vowels = Just (Word' s)
     | otherwise = Nothing
       where vowels = countVowels s
-- It's only Natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero       = 0
natToInteger (Succ nat) = 1 + natToInteger nat

integerToNat :: Integer -> Maybe Nat
integerToNat n
          | n < 0 = Nothing
          | otherwise = go n Zero
            where go 0 nat = Just nat
                  go x nat = go (x-1) (Succ nat)

-- Small library for Maybe 1
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing Nothing  = True
isNothing (Just _) = False
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee z _ Nothing  = z
-- 3
fromMaybe :: a -> Maybe a -> a
fromMaybe a ma = mayybee a id ma
-- 4
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (a:_) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]
-- 5
catMaybes :: [Maybe a] -> [a]
catMaybes ma = map (\(Just x) -> x) (filter isJust ma)

catMaybes' :: [Maybe a] -> [a]
catMaybes' = foldr (\a as -> case a of
  (Just x) -> x:as
  _        -> as) []

catMaybes'' :: [Maybe a] -> [a]
catMaybes'' ma = do
  Just x <- ma
  return x
-- in Data.Maybe
catMaybes1 :: [Maybe a] -> [a]
catMaybes1 ls = [x | Just x <- ls]

{- in Scala
def isSome[A](o: Option[A]): Boolean = o match {
  case Some(_) => true
  case _ => false

def catMaybes[A](opt: List[Option[A]]): List[A] =
  (opt.filter(isSome)).map { case Some(x) => x }

def catMaybes[A](opt: List[Option[A]]): List[A] =
  opt.foldRight(List[A]())( (a, b) => a match {
    case Some(x) => x :: b
    case None => b
  })

def catMaybes[A](opt: List[Option[A]]): List[A] = for {
  Some(x) <- opt
}yield x
-}
-- 6
flipMaybe :: (Eq a) => [Maybe a] -> Maybe [a]
flipMaybe ma
        | Nothing `elem` ma = Nothing
        | otherwise = Just (catMaybes'' ma)

-- Small library for Either 1
lefts' :: [Either a b] -> [a]
lefts' = foldr (lefties) []
  where lefties (Left a) as = a : as
        lefties _ as        = as
-- 2
rights' :: [Either a b] -> [b]
rights' = foldr (righties) []
  where righties (Right b) bs = b : bs
        righties _ bs         = bs
-- 3
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' eithers = (lefts' eithers, rights' eithers)
-- 4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right b) = Just (f b)
-- 5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' ac _ (Left a)  = ac a
either' _ bc (Right b) = bc b
-- 6
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' bc e = either' (\_ -> Nothing) (\b -> Just (bc b)) e

-- Unfolds
myIterate :: (a -> a) -> a -> [a]
myIterate f z = z : myIterate f (f z)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f z = case f z of
  Nothing     -> []
  Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myUnfoldr (\a -> Just (a, f a)) z

-- unfolds for Tree 1
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f z = case f z of
  Nothing        -> Leaf
  Just (l, b, r) -> Node (unfold f l) b (unfold f r)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold build 0
  where build x
          | x == n = Nothing
          | otherwise = Just (x+1, x, x+1)
