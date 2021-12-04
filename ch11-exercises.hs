

module Ch11Exercises where

import Data.Char
import Data.List

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
capitalizeWord (x:xs) = toUpper x : xs
capitalizeWord [] = []

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

-- Phone exercise
-- 1.
newtype DaPhone = DaPhone [(Char, String)]
             deriving (Eq, Show)

daPhone = DaPhone [ ('0', " 0")
                  , ('1', "1")
                  , ('2', "ABC2")
                  , ('3', "DEF3")
                  , ('4', "GHI4")
                  , ('5', "JKL5")
                  , ('6', "MNO6")
                  , ('7', "PQRS7")
                  , ('8', "TUV8")
                  , ('9', "WXYZ9")
                  , ('*', "^")
                  , ('#', ".,") ]

-- 2.
convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn" ]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: DaPhone
            -> Char
            -> [(Digit, Presses)]
reverseTaps (DaPhone ks) c =
  if isUpper c
  then ('*', 1) : [(fst t, n)]
  else [(fst t, n)]
    where
      t = head $ filter (\(f,s) -> toUpper c `elem` s) ks
      n = 1 + length (takeWhile (/= toUpper c) (snd t))

cellPhonesDead :: DaPhone
               -> String
               -> [(Digit, Presses)]
cellPhonesDead daPhone = concatMap $ reverseTaps daPhone

-- 3.
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(a1, a2) b -> a2 + b) 0

-- 4.
mostPopular :: Ord a => [a] -> a
mostPopular = head . last . sortOn length . group . sort

mostPopularLetter :: String -> Char
mostPopularLetter = mostPopular . filter (\c -> c `notElem` " .,")

cost :: DaPhone -> Char -> Int
cost p = fingerTaps . reverseTaps p

-- 5.
coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String
coolestWord = mostPopular . words . concat

-- Hutton's razor
-- 1.
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add a b) = (+) (eval a) (eval b)

-- 2.
printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add a b) = printExpr a ++ " + " ++ printExpr b
