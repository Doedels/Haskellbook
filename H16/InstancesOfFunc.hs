module InstancesOfFunc where

import Test.QuickCheck
import Test.QuickCheck.Function

-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  x <- arbitrary
  return (Identity x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

type CompIdentity = Identity Int -> Fun Int Int -> Fun Int Int -> Bool

-- 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

genPair :: Arbitrary a => Gen (Pair a)
genPair = do
  x <- arbitrary
  return (Pair x x)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = genPair

type CompPair = Pair Int -> Fun Int Int -> Fun Int Int -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

genTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
genTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = genTwo

type CompTwo = Two Int Int -> Fun Int Int -> Fun Int Int -> Bool

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

genThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
genThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = genThree

type CompThree = Three Int Int Int -> Fun Int Int -> Fun Int Int -> Bool

-- 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

genThree' :: (Arbitrary a, Arbitrary b) => Gen (Three' a b)
genThree' = do
  a <- arbitrary
  b <- arbitrary
  return (Three' a b b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = genThree'

type CompThree' = Three' Int Int -> Fun Int Int -> Fun Int Int -> Bool

-- 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

genFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
genFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = genFour

type CompFour = Four Int Int Int Int -> Fun Int Int -> Fun Int Int -> Bool

-- 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

genFour' :: (Arbitrary a, Arbitrary b) => Gen (Four' a b)
genFour' = do
  a <- arbitrary
  b <- arbitrary
  return (Four' a a a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = genFour'

type CompFour' = Four' Int Int -> Fun Int Int -> Fun Int Int -> Bool

-- 8 Can you implement one for this type? Why? Why not?
data Trivial = Trivial deriving (Eq, Show)
-- No valid Functor, because there is only structure and no falues to map over

-- *************************************
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose' :: CompIdentity)

  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose' :: CompPair)

  quickCheck (functorIdentity :: Two Int Int -> Bool)
  quickCheck (functorCompose' :: CompTwo)

  quickCheck (functorIdentity :: Three Int Int Int -> Bool)
  quickCheck (functorCompose' :: CompThree)

  quickCheck (functorIdentity :: Three' Int Int -> Bool)
  quickCheck (functorCompose' :: CompThree')

  quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
  quickCheck (functorCompose' :: CompFour)

  quickCheck (functorIdentity :: Four' Int Int -> Bool)
  quickCheck (functorCompose' :: CompFour')
