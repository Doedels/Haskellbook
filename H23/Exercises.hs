{-# LANGUAGE InstanceSigs #-}

module Exercises where

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \s -> case g s of (a, s') -> (f a, s')

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ \s -> case g s of (a, s') -> ( fst (f s) a, s')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ \s -> (case f s of (a, s') -> runMoi (g a) s')

-- Chapter exercises
-- 1
get :: Moi s s
get = Moi $ \s -> (s, s)

-- 2
put :: s -> Moi s ()
put s = Moi $ \_ -> ((), s)

-- 3
exec :: Moi s a -> s -> s
exec (Moi sa) s = snd (sa s)

-- 4
eval :: Moi s a -> s -> a
eval (Moi sa) s = fst (sa s)

-- 5
modify :: (s -> s) -> Moi s ()
modify f = get >>= put . f
