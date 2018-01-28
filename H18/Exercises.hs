module Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Monad

-- 18.4 Examples of Monad use
-- Short Exercise: Either Monad
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First a <*> First _ = First a
  First a <*> Second _ = First a
  Second _ <*> First a = First a
  Second f <*> Second b = Second (f b)

instance Monad (Sum a) where
  return = pure
  First a >>= _ = First a
  Second b >>= f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = oneof [First <$> arbitrary, Second <$> arbitrary]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

-- Chapter Exercises
-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

-- 2  I took the 't' off Left and Right to distinguish from the Left and Right in prelude
data PhhhbbtttEither b a = Lef a | Righ b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap _ (Righ b) = Righ b
  fmap f (Lef a) = Lef (f a)

instance Applicative (PhhhbbtttEither b) where
  pure = Lef
  Righ b <*> _ = Righ b
  _ <*> Righ b = Righ b
  Lef f <*> Lef a = Lef (f a)

instance Monad (PhhhbbtttEither b) where
  return = pure
  Righ b >>= _ = Righ b
  Lef a >>= f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = oneof [Lef <$> arbitrary, Righ <$> arbitrary]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

-- 3
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 4
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

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

instance Monad List where
  return = pure
  xs >>= f = flatMap f xs

genCons :: Arbitrary a => Gen (List a)
genCons = do
  x <- arbitrary
  return (Cons x Nil)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, pure Nil), (10, genCons)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- *********************************
main :: IO ()
main = do
  quickBatch (monad (undefined :: Sum (String, Char, Int) (String, Char, Int)))
  quickBatch (monad (undefined :: Nope (String, Char, Int)))
  quickBatch (monad (undefined :: PhhhbbtttEither (String, Char, Int) (String, Char, Int)))
  quickBatch (monad (undefined :: Identity (String, Char, Int)))
  quickBatch (monad (undefined :: List (String, Char, Int)))

-- Write the following functions using methods provided by Monad and Functor
-- 1
j :: Monad m => m (m a) -> m a
j = join
-- 2
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
-- 3
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2
-- 4
a1 :: Monad m => m a -> m (a -> b) -> m b
a1 ma mf = ap mf ma -- mf <*> ma
-- 5
meh :: Monad m => [a] -> (a -> m b) -> m [b]
-- meh xs f = mapM f xs  |  traverse f xs
meh xs f = go xs f (pure [])
  where go [] _ zs = zs
        go (y:ys) g zs = go ys g ((:) <$> (g y) <*> zs)
-- 6
flipType :: Monad m => [m a] -> m [a]
-- flipType = sequence  |  sequenceA
flipType lma = meh lma id
