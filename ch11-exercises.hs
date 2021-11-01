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

-- Programmers

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgLang }
  deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgLang]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = getProgrammers allOperatingSystems allLanguages
  where getProgrammers oss langs =
          [Programmer {os = x', lang = y'} | x' <- oss, y' <- langs]
