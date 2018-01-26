module PropertyTests where

import Data.Monoid
import Control.Applicative
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

-- testBull is supposed to fail left- and right identity
testBull :: IO ()
testBull = quickBatch (monoid Twoo)

-- *** ZipList exercise ***
instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

--instance Arbitrary for ZipList and Sum already exist in Test.QuickCheck.Arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

testZipList :: IO ()
testZipList = quickBatch (monoid (ZipList [1 :: Sum Int]))

-- *** List Applicative Exercise ***
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  xs <*> ys = flatMap (<$> ys) xs

g :: List (Integer -> Integer)
g = Cons (+1) (Cons (*2) Nil)

v :: List Integer
v = Cons 1 (Cons 2 Nil)

listApp :: List Integer
listApp = g <*> v  -- should be Cons 2(Cons 3 (Cons 2(Cons 4 Nil)))

-- *** Ziplist Applicative Exercise ***
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure a = ZipList' (repeat' a)
  ZipList' xs <*> ZipList' ys = ZipList' (zipWith' ($) xs ys)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)
zipWith' _ _ _                     = Nil

-- *** Exercise: Variations on Either ***
data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  Failure e <*> Failure ee = Failure (e <> ee)
  Failure e <*> Success _ = Failure e
  Success _ <*> Failure e = Failure e
  Success f <*> Success a = Success (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = oneof [(Failure <$> arbitrary), (Success <$> arbitrary)]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

validationTestCase :: Validation String (String, Char, Integer)
validationTestCase = Success ("a", 'b', 1)

testValidation :: IO ()
testValidation = quickBatch (applicative validationTestCase)

{- *** Chapter Exercises ***
1
pure :: a -> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]

2
pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

3
pure :: a -> (a, a)
(<*>) :: (a, (a -> b))  -> (a, a) -> (a, b)

4
(->) e
-- Methods
pure :: a -> (e -> a)
(<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
-}

-- Write instances for the following datatypes
-- 1
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  Pair f g <*> Pair a b = Pair (f a) (g b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  Two a f <*> Two b c = Two (a <> b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  Three a b f <*> Three c d e = Three (a <> c) (b <> d) (f e)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure a = Three' mempty a a
  Three' a f g <*> Three' b c d = Three' (a <> b) (f c) (g d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure a = Four mempty mempty mempty a
  Four a b c f <*> Four a' b' c' d = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

-- 6
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure a = Four' mempty mempty mempty a
  Four' a b c f <*> Four' a' b' c' d = Four' (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

main :: IO ()
main = do
  -- quickBatch (functor (Pair ("a", 'b', 1 :: Int) ("a", 'b', 1 :: Int)))
  -- quickBatch (applicative (Pair ("a", 'b', 1 :: Int) ("a", 'b', 1 :: Int)))
  -- or:
  quickBatch (functor (undefined :: Pair (String, Int, Bool)))
  quickBatch (applicative (undefined :: Pair (String, Int, Bool)))

  quickBatch (functor (Two "a" ("a", 'b', 1 :: Int)))
  quickBatch (applicative (Two "a" ("a", 'b', 1 :: Int)))

  quickBatch (functor (Three "a" "b" ("a", 'b', 1 :: Int)))
  quickBatch (applicative (Three "a" "b" ("a", 'b', 1 :: Int)))

  quickBatch (functor (Three' "a" ("a", 'b', 1 :: Int) ("a", 'b', 1 :: Int)))
  quickBatch (applicative (Three' "a" ("a", 'b', 1 :: Int) ("a", 'b', 1 :: Int)))

  quickBatch (functor (Four "a" "b" [1 :: Int] ("a", 'b', 1 :: Int)))
  quickBatch (applicative (Four "a" "b" [1 :: Int] ("a", 'b', 1 :: Int)))

  quickBatch (functor (Four' "a" "b" "c" ("a", 'b', 1 :: Int)))
  quickBatch (applicative (Four' "a" "b" "c" ("a", 'b', 1 :: Int)))

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

showCombos :: [String]
showCombos = (\x -> case x of (a, b, c) -> [a, b, c]) <$> combos stops vowels stops
