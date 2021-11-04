-- |

module Ch11Exercises where

import Data.Char

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

-- !! it's very important not to propagate bottoms when dealing with record syntax:

johnnyProg = Programmer { os = GnuPlusLinux} -- this will lead to runtime errors!!

-- if a Product type value needs to be constructed without all values,
-- follow the strategy below. "Percolate values, not bottoms!"

data ThereYet =
  There Float Int Bool
  deriving (Eq, Show)

notYet :: Int -> Bool -> ThereYet
notYet = There 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yussss :: ThereYet
yussss = notQuite False

-- Chapter Exercises
-- Ciphers
cipher :: String -> String -> String
cipher xs w =
  go xs w f 0 where
  go [] _ _ _ = []
  go (x:xs) w f c = f x w c : go xs w f (c + 1)
  f x w c = chr $ ord x + (ord (toUpper $ w !! mod c (length w)) - ord 'A')

decipher :: String -> String -> String
decipher xs w =
  go xs w f 0 where
  go [] _ _ _ = []
  go (x:xs) w f c = f x w c : go xs w f (c + 1)
  f x w c = chr $ ord x - (ord (toUpper $ w !! mod c (length w)) - ord 'A')

-- As-patterns
-- 1.
isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf a@(x:xs) b@(y:ys)
  | x /= y = isSubseqOf a ys
  | x == y = isSubseqOf xs ys

-- 2.
capitalizeWords :: String
                -> [(String, String)]
capitalizeWords = map caps . words
  where
    caps w@(x:xs) = (w, toUpper x : xs)

-- Language exercises
-- 1.
capitalizeWord :: String -> String
capitalizeWord w@(x:xs) = toUpper x : xs

-- 2.
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph p =
  capitalizeWord (take n p) ++ capitalizeParagraph (drop n p)
    where
      n = go p 0
      go [] c = c
      go (x:xs) c
        | x == '.' = c + 2
        | otherwise = go xs (c + 1)
