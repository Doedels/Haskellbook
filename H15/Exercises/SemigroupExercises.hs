module SemigroupExercises where

import Data.Semigroup
import Test.QuickCheck

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

genIdentity :: Arbitrary a => Gen (Identity a)
genIdentity = do
  x <- arbitrary
  return (Identity x)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = genIdentity

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

getTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
getTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = getTwo

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d e f) = Three (a <> d) (b <> e) (c <> f)

getThree :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
getThree = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = getThree

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

-- 5
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

getFour :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
getFour = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  d <- arbitrary
  return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = getFour

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

-- 6
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _ <> BoolConj _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = elements [(BoolConj True), (BoolConj False)]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

 -- 7
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  BoolDisj _ <> BoolDisj _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = elements [(BoolDisj True), (BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Fst _ <> Fst x = Fst x
  Snd x <> Snd _ = Snd x
  Fst _ <> Snd x = Snd x
  Snd x <> Fst _ = Snd x

genOr :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
genOr = do
  a <- arbitrary
  b <- arbitrary
  elements [Fst a, Snd b]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = genOr

type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool

-- 9 Combine a b does NOT take two arguments, but ONE that is a function a -> b
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (\a -> f a <> g a)

-- 10
newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

{-
Skipped the test for exercises 9 and 10
-}

-- *******************************************
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
