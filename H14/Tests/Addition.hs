module Addition where

import Test.Hspec
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
-- multiplication by recursive summing from the chapter 8 exercises
multi :: (Integral a) => a -> a -> a
multi _ 0 = 0
multi n m = n + multi n (m - 1)

testMulti :: IO ()
testMulti = hspec $ do
  describe "multi" $ do
    it "3 * 3 = 9" $ do
      multi 3 3 `shouldBe` 9
    it "0 * 10 = 0" $ do
      multi 0 10 `shouldBe` 0
    it "10 * 0 = 0" $ do
      multi 10 0 `shouldBe` 0

testDividedBy :: IO ()
testDividedBy = hspec $ do
  describe "dividedBy" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is (4, 2)" $ do
      dividedBy 22 5 `shouldBe` (4, 2)

main :: IO ()
main = hspec $ do
  describe "addition" $ do
    it "1 + 1 is greater than 1" $ do
      1 + 1 > 1 `shouldBe` True
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

trivialInt :: Gen Int
trivialInt = return 1

oneTroughThree :: Gen Int
oneTroughThree = elements [1,2,3]

oneTroughThree' :: Gen Int
oneTroughThree' = elements [1,2,2,2,2,3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- higher 'Just' chances
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a)) ]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
