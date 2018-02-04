module ChapterExercises where

import Data.Monoid

-- Write Foldabele instances for the following datatypes
-- 1
data Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- 2
data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 3
data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4
data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b c) = f b <> f c

-- 5
data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldMap f (Four' _ b c d) = f b <> f c <> f d

-- Filter
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then pure x else mempty)
