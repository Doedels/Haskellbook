{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import GHC.Arr

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

-- 4
newtype Mu f = Inf { outF :: f (Mu f) }
-- Mu :: (* -> *) -> *
-- Inf :: f (Mu f) -> Mu f

-- instance Functor (Mu a) where
-- Mu a replaces (* -> *) which leaves kind * and Functor needs * -> *
-- So no possible Functor instance FlexibleInstances

-- 5
data D = D (Array Word Word) Int Int





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

-- 4
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a = LiftItOut (f a)
-- f a = a Functor over a !
instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut (fmap g x)

-- 6


-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething f g) = IgnoringSomething f (fmap h g)

-- 8



-- 9
data List a = Nil | Cons a (List a)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)
