module MonoidExercises where

import Data.Semigroup
import Test.QuickCheck

-- 1
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  Identity a `mappend` Identity b = Identity (a `mappend` b)

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

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  (Two a b) `mappend` (Two c d) = Two (a `mappend` c) (b `mappend` d)

getTwo :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
getTwo = do
  a <- arbitrary
  b <- arbitrary
  return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = getTwo

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

-- 4
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  BoolConj _ <> BoolConj _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  BoolConj True `mappend` BoolConj True = BoolConj True
  BoolConj _ `mappend` BoolConj _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = elements [BoolConj True, BoolConj False]

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

 -- 5
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj False <> BoolDisj False = BoolDisj False
  BoolDisj _ <> BoolDisj _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False
  BoolDisj False `mappend` BoolDisj False = BoolDisj False
  BoolDisj _ `mappend` BoolDisj _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = elements [(BoolDisj True), (BoolDisj False)]

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6


-- 7


-- *******************************************
semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidAssoc :: (Monoid m, Eq m) => m -> m -> m -> Bool
monoidAssoc a b c = (a `mappend` (b `mappend` c)) == ((a `mappend` b) `mappend` c)

monoidLeftID :: (Monoid a, Eq a) => a -> Bool
monoidLeftID a = mempty `mappend` a == a

monoidRightID :: (Monoid a, Eq a) => a -> Bool
monoidRightID a = a `mappend` mempty == a

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidAssoc :: TrivAssoc)
  quickCheck (monoidLeftID :: Trivial -> Bool)
  quickCheck (monoidRightID :: Trivial -> Bool)

  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidAssoc :: IdentityAssoc)
  quickCheck (monoidLeftID :: Identity String -> Bool)
  quickCheck (monoidRightID :: Identity String -> Bool)

  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidAssoc :: TwoAssoc)
  quickCheck (monoidLeftID :: Two String String -> Bool)
  quickCheck (monoidRightID :: Two String String -> Bool)

  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftID :: BoolConj -> Bool)
  quickCheck (monoidRightID :: BoolConj -> Bool)

  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftID :: BoolDisj -> Bool)
  quickCheck (monoidRightID :: BoolDisj -> Bool)
