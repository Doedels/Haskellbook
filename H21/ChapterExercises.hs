{-# LANGUAGE FlexibleContexts #-}

module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid

-- 1
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> r = fmap f r

instance Foldable Identity where
  foldMap f (Identity a) = f a
  foldr f z (Identity a) = f a z

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- 2
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  Constant a <*> Constant b = Constant (a <> b)

instance Foldable (Constant a) where
  foldr _ z (Constant _) = z

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

-- 3
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
  pure = Yep
  Yep f <*> Yep a = Yep (f a)
  _ <*> _ = Nada

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = oneof [pure Nada, Yep <$> arbitrary]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- 4
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    xs <- listOf arbitrary
    pure (foldr Cons Nil xs)

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- 5
data Three a b c = Three a b c deriving (Eq, Ord, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- 6
data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- 7
data Big a b = Big a b b deriving (Eq, Ord, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- 8
data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- 9
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance (Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

instance (Eq (n a), Eq a) => EqProp (S n a) where
  (=-=) = eq

-- 10
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty         = pure Empty
  traverse f (Leaf a)      = Leaf <$> f a
  traverse f (Node l x r)  = Node <$> traverse f l <*> f x <*> traverse f r

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [pure Empty, Leaf <$> arbitrary, Node <$> arbitrary <*> arbitrary <*> arbitrary]

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

-- ****************************************************
main :: IO ()
main = do
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: Optional (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: List (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: Pair (Int, Int, [Int]) (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: Big (Int, Int, [Int]) (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: Bigger (Int, Int, [Int]) (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: S Maybe (Int, Int, [Int]) ))
  quickBatch (traversable (undefined :: Tree (Int, Int, [Int]) ))
