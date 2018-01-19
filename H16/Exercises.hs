{-# LANGUAGE FlexibleInstances #-}

module Exercises where

import GHC.Arr

-- Determine if a valid Functor can be written
-- 1
data Bool = False | True deriving (Eq, Show)
-- No valid Functor, because there is only structure and no falues to map over

-- 2
data BoolAndSomethingElse a = False' a | True' a deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- 3
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- 4
newtype Mu f = Inf { outF :: f (Mu f) }
-- Mu :: (* -> *) -> *
-- Inf :: f (Mu f) -> Mu f

-- instance Functor (Mu a) where
-- Mu a replaces (* -> *) which leaves kind * and Functor needs * -> *
-- So no possible Functor instance

-- 5
data D = D (Array Word Word) Int Int  deriving (Eq, Show)
-- D has all concrete type constructor ant thus kind *
-- Functor needs kind * -> *, So no possible Functor instance

-- Rearrange the Arguments .....
-- 1
data Sum b a = First a | Second b deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap _ (Second b) = Second b

-- 2
data Company a c b = DeepBlue a c | Something b deriving (Eq, Show)

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
data Quant a b = Finance | Desk a | Floor b deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Floor b) = Floor (f b)

-- 2
newtype K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K x)) = Flip (K (f x))

-- 4
data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

-- 5
data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)
-- f a = a Functor over a !
instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut x) = LiftItOut (fmap g x)
  -- x represent (f a) here, so we have to fmap over the additional context

-- 6
data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)
  -- DaWrappa takes 2 arguments which are both values inside a Functorial context
  -- So, as with exercise 5 (LiftItOut) we have to fmap over this additional context

-- 7
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)

-- 8
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

-- 9
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- 10
data GoatLord a = NoGoat
                | OneGoat a
                | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
                deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

-- 11
data TalkToMe a = Halt
                | Print String a
                | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print st a) = Print st (f a)
  fmap f (Read stToA) = Read (fmap f stToA)
