module Exercises where

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
