{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Goats where

class TooMany a where
  tooMany :: a -> Bool
instance TooMany Int where
  tooMany n = n > 42
--instance TooMany (Int, String) where
--  tooMany (n, _) = n > 42
--instance TooMany (Int, Int) where
--  tooMany (n, m) = (n + m) > 42
instance (Num a, Ord a, TooMany a) => TooMany (a, a) where
  tooMany (n, m) = n + m > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype Cows = Cows Int deriving (Eq, Show, TooMany)

newtype Pigs = Pigs Int deriving (Eq, Show)
instance TooMany Pigs where
  tooMany (Pigs n) = n > 21
