module Vehicles where

newtype Price = Price Int deriving (Eq, Show)

data Size = Narrow | Wide | Heavy | Jumbo deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir
             | CatapultsRUs
             | TakeYourChancesUnited
             deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Size Airline
             deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane Narrow PapuAir
-- 2
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar
-- 3
getManu :: Vehicle -> Maybe Manufacturer
getManu (Car x _) = Just x
getManu _         = Nothing
