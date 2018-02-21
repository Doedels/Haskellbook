{-# LANGUAGE InstanceSigs #-}

module Exercises where

newtype Reader r a = Reader { runReader :: r -> a }

-- Exercise: Ask
ask :: Reader a a
ask = Reader id

-- Exercise: Reading Comprehension
-- 1
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

-- 2
asks :: (r -> a) -> Reader r a
asks = Reader

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \r -> f (ra r)
                    -- Reader $ (f . ra)
-- 3
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
--pure = Reader . const

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> rab r (ra r)

-- Exercise: Reader Monad
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRrb = Reader $ \r -> runReader (aRrb (ra r)) r
