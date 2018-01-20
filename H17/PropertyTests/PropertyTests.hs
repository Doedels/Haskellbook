module PropertyTests where

import Data.Monoid
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

instance EqProp Bull where
  (=-=) = eq

-- testBull is supposed to fail left- and right identity
testBull :: IO ()
testBull = quickBatch (monoid Twoo)

-- *** ZipList exercise ***
instance Monoid a => Monoid (ZipList a) where
  mempty = pure mempty
  mappend = liftA2 mappend

--instance Arbitrary for ZipList and Sum already exist in Test.QuickCheck.Arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

testZipList :: IO ()
testZipList = quickBatch (monoid (ZipList [1 :: Sum Int]))

-- *** List Applicative Exercise ***
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  xs <*> ys = flatMap (<$> ys) xs

g :: List (Integer -> Integer)
g = Cons (+1) (Cons (*2) Nil)

v :: List Integer
v = Cons 1 (Cons 2 Nil)

listApp :: List Integer
listApp = g <*> v  -- should be Cons 2(Cons 3 (Cons 2(Cons 4 Nil)))

-- *** Ziplist Applicative Exercise ***
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs in take' 3000 l
          ys' = let (ZipList' l) = ys in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure a = ZipList' (repeat' a)
  ZipList' xs <*> ZipList' ys = ZipList' (zipWith' ($) xs ys)

repeat' :: a -> List a
repeat' x = Cons x (repeat' x)

zipWith' :: (a -> b -> c) -> List a -> List b -> List c
zipWith' f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith' f xs ys)
zipWith' _ _ _                     = Nil
