module Optional where

import Control.Monad
import Data.Monoid
import Test.Hspec
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only (a `mappend` b)

testOptional :: IO ()
testOptional = hspec $ do
  describe "Optional" $ do
    it "Only (Sum 1) `mappend` Only (Sum 1)" $ do
      Only (Sum 1) `mappend` Only (Sum 1) `shouldBe` Only (Sum {getSum = 2})
    it "Only (Product 4) `mappend` Only (Product 2)" $ do
      Only (Product 4) `mappend` Only (Product 2) `shouldBe` Only (Product {getProduct = 8})
    it "Nada `mappend` Only (Sum 1)" $ do
      Nada `mappend` Only (Sum 1) `shouldBe` Only (Sum 1)
    it "Only (Sum 2) `mappend` Nada" $ do
      Only (Sum 2) `mappend` Nada `shouldBe` Only (Sum 2)
    it "Only [1,2,3] <> Only [4,5,6]" $ do
       Only [1,2,3] <> Only [4,5,6] `shouldBe` Only [1,2,3,4,5,6]

monoidAssoc :: (Monoid m, Eq m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftID :: (Monoid a, Eq a) => a -> Bool
monoidLeftID a = mempty <> a == a

monoidRightID :: (Monoid a, Eq a) => a -> Bool
monoidRightID a = a <> mempty == a

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

genFirst :: Arbitrary a => Gen (First' a)
genFirst = do
  x <- arbitrary
  frequency [ (1, return (First' Nada))
            , (10, return (First' (Only x) ) ) ]
-- to get a sample of arbitrary values, use in ghci:
-- sample (genFirst :: Gen (First' Int))

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' Nada) = First' Nada
  mappend (First' Nada) (First' (Only b)) = First' (Only b)
  mappend (First' (Only a)) (First' Nada) = First' (Only a)
  mappend (First' (Only a)) (First' (Only _)) = First' (Only a)

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftID :: FstId)
  quickCheck (monoidRightID :: FstId)
