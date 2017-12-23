module Date where

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show)
-- Fri is always greater, Other Days are equals
instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ

instance Eq DayOfWeek where
  Mon == Mon = True
  Tue == Tue = True
  Wed == Wed = True
  Thu == Thu = True
  Fri == Fri = True
  Sat == Sat = True
  Sun == Sun = True
  _ == _ = False

data Date = Date DayOfWeek Int deriving (Show)

instance Eq Date where
  (Date x y) == (Date x' y') =
    x == x' && y == y'
