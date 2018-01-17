{-# LANGUAGE FlexibleInstances #-}

module Exercises where

-- Determine if a valid Functor can be written
-- 1
data Bool = False | True
-- No valid Functor, because there is only structure and no falues to map over
-- 2
data BoolAndSomethingElse a = False' a | True' a deriving Show

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)
-- 3
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving Show

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)
-- Rearrange the Arguments .....
-- 1
data Sum b a = First a | Second b deriving Show

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b
-- 2
data Company a c b = DeepBlue a c | Something b deriving (Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c
-- 3
data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'
-- Write Functor instances ........
-- 1
data Quant a b = Finance | Desk a | Floor b deriving (Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Floor b) = Floor (f b)
-- 2
newtype K a b = K a deriving (Show)

instance Functor (K a) where
  fmap _ (K a) = K a
-- 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))
