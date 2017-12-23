module WherePenguinsLive where

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Austalia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: Penguin -> Bool
isSouthAfrica (Peng SouthAfrica) = True
isSouthAfrica                  _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives
