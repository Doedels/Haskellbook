module DBase where

import           Data.Time

data DataBaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

db :: [DataBaseItem]
db = [ DbDate (UTCTime(fromGregorian 1911 5 1) (secondsToDiffTime 34123))
     , DbNumber 9001
     , DbString "Hello world!"
     , DbDate (UTCTime(fromGregorian 1921 5 1) (secondsToDiffTime 34123))
     ]
-- 1
filterDbDate :: Foldable t => t DataBaseItem -> [UTCTime]
filterDbDate = foldr getDbDate []
  where getDbDate a b = case a of
           DbDate date -> date : b
           _           -> b
-- 2
filterDbNumber :: Foldable t => t DataBaseItem -> [Integer]
filterDbNumber = foldr getDbNr []
  where getDbNr a b = case a of
          DbNumber nr -> nr : b
          _           -> b
-- 3
mostRecent :: Foldable t => t DataBaseItem -> UTCTime
mostRecent = maximum . filterDbDate
-- 4
sumDb :: Foldable t => t DataBaseItem -> Integer
sumDb = sum . filterDbNumber
-- 5
avgDb :: (Foldable t, Fractional a) => t DataBaseItem -> a
avgDb xs = fromIntegral (sumDb xs)
           / fromIntegral (length (filterDbNumber xs))
