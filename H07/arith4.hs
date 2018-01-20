module Arith4 where
-- exercise 4
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main :: IO ()
main = do
  print (roundTrip 4)
  print (id 4)

-- 5 pointfree roundTrip
roundTrip1 :: (Show a, Read a) => a -> a
roundTrip1 = read . show

main1 :: IO ()
main1 = do
  print (roundTrip1 4)
  print (id 4)

-- 6
roundTrip2 :: (Show a, Read b) => a -> b
roundTrip2 = read . show

main2 :: IO ()
main2 = do
  print (roundTrip2 4 :: Int)
  print (id 4)
