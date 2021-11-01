-- |

module Ch11Exercises where

-- Exercises: Vehicles

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini
  | Mazda
  | Tata
  | None deriving (Eq, Show)

data Airline = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited deriving (Eq, Show)

type Size = Integer

data Vehicle = Car Manufacturer Price
  | Plane Airline Size deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir 10

-- 1.
-- Type of myCar is Vehicle

-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] ->  [Bool]
areCars = map isCar

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu _ = None
