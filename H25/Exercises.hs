{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Data.Char
import Prelude hiding (Either, Left, Right)

newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)
{-          :k Identity :: * -> *
This is the function id :: a -> a on the kinds level -}

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)
{-            :k Compose :: (* -> *) -> (* -> *) -> * -> *
this is the function (.) :: (b -> c) -> (a -> b) -> a -> c on the kinds level
f and g must be a * -> * type constructors (like Maybe or [])
and a must be a (*) concrete type (like Char)
:t Compose Just (Identity 'a')
:: Compose Maybe Identity Char
          (*->*)->(*->*) -> *
Compose Just (Identity 'a')
Compose {getCompose = Just (Identity {runIdentity = 'a'})}           -}

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga
{- We are fmap-ing over two structures here (f and g) to get to the value a
ys = Just (Identity 'a')
fmap toUpper (Compose ys)
Compose {getCompose = Just (Identity {runIdentity = 'A'})}              -}

newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One fa) = One $ fmap f fa

newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

-- this function is called v in the book
nestedCompose :: Compose [] Maybe (Compose Maybe [] Integer)
nestedCompose = Compose [Just (Compose $ Just [1])]

-- some extra personal test cases, not in the book
nestedIdentity :: Identity (Identity (Identity String))
nestedIdentity = Identity (Identity (Identity "multiple Identities"))

liftOverFourContexts :: Identity (Identity (Identity [Char]))
liftOverFourContexts = (fmap . fmap .fmap . fmap) toUpper nestedIdentity
-- there are 4 structures to fmap over, 3 times Identity and the list in [Char]

-- 25.4 Twinplivative exercise
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose (pure . pure $ a)

  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose $ (<*>) <$> f <*> a

-- 25.6 Exercises: Compose Instances
-- 1
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- 2
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

-- Bifunctor Exercise
class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

-- 1
data Duex a b = Duex a b deriving (Eq, Show)

instance Bifunctor Duex where
  bimap f g (Duex a b) = Duex (f a) (g b)

-- 2
data Const a b = Const a deriving (Eq, Show)

instance Bifunctor Const where
  bimap f g (Const a) = Const (f a)

-- 3
data Drei a b c = Drei a b c deriving (Eq, Show)

instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4
data SuperDrei a b c = SuperDrei a b deriving (Eq, Show)

instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

-- 5
data SemiDrei a b c = SemiDrei a deriving (Eq, Show)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

-- 6
data Quadriceps a b c d = Quadzzz a b c d deriving (Eq, Show)

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

-- 7
data Either a b = Left a | Right b deriving (Eq, Show)

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right b) = Right (g b)

-- IdentityT.
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)
-- The f signals additional (monadic) structure

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)


instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
  pure a = IdentityT (pure a)
  IdentityT mf <*> IdentityT mb = IdentityT (mf <*> mb)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a

instance Monad m => Monad (IdentityT m) where
  return = pure
  (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
  IdentityT ma >>= f = IdentityT (ma >>= runIdentityT . f)     -- f :: a -> Identity m b
--                               m a >>= (a -> m b) -> m b  runIdentityT . f :: a -> m b
