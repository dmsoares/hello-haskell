-- |

module CH10Exercises where

import Data.Time
import System.Posix (BaudRate(B1200))

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 50000))
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 1
  , DbNumber 5
  ]

-- Database
-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbItems = [item | (DbDate item) <- dbItems]

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dbItems = [item | (DbNumber item) <- dbItems]

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb dbItems = fromIntegral (sum dbNumbers) / fromIntegral (length dbNumbers)
  where dbNumbers = filterDbNumber dbItems

-- Scans exercises
fibs = 1 : scanl (+) 1 fibs

-- 1.
fibs20 = take 20 $ 1 : scanl (+) 1 fibs20

-- 2.
fibsLT100 = takeWhile (<100) $ 1 : scanl (+) 1 fibsLT100

-- 3.
factorial = scanl (*) 1 [1..]

-- Chapter Exercises
-- Warm-up and review
-- 1.a
stops = "pbtdkg"
vowels = "aeiou"

f = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

-- 1.b
f' = [(x,y,z) | x <- stops, x == 'p', y <- vowels, z <- stops]

-- 1.c
nouns = ["cat", "dog", "horse"]
verbs = ["eat", "sleep", "play", "drink"]

f'' = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]
f3 = zip3

-- 2.
-- Word length average
seekritFunc x = div (sum (map length (words x)))
                    (length (words x))

-- 3.
seekritFunc' x = (/) (fromIntegral (sum (map length (words x))))
                    (fromIntegral (length (words x)))

-- Rewriting functions using folds
-- 1.
bools = [True, False, False, False]
myOr0 :: [Bool] -> Bool
myOr0 = foldr (\x y -> if x then True else y) False

myOr1 :: [Bool] -> Bool
myOr1 xs = foldr (||) False xs

myOr2 :: [Bool] -> Bool
myOr2 = foldr (||) False

-- 2.
myAny0 :: (a -> Bool) -> [a] -> Bool
myAny0 f = foldr (\x y -> if f x then True else y) False

myAny1 :: (a -> Bool) -> [a] -> Bool
myAny1 f = foldr (\x y -> f x || y) False

-- 3.
myElem0 :: Eq a => a -> [a] -> Bool
myElem0 a = foldr (\x y -> x == a || y) False

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 a = myAny1 (== a)

-- 4.
myReverse0 :: [a] -> [a]
myReverse0 = foldl (flip (:)) []

-- 5.
myMap0 :: (a -> b) -> [a] -> [b]
myMap0 f = foldr (\x y -> f x : y) []

myMap1 :: (a -> b) -> [a] -> [b]
myMap1 f = foldr ((:) . f) []

-- 6.
myFilter0 :: (a -> Bool) -> [a] -> [a]
myFilter0 f = foldr (\x y -> if f x then x : y else y) []

myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 f xs = [x | x <- xs, f x]

-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10.
myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy f = head . foldr (\x y ->
                                case y of
                                  [] -> [x]
                                  _ -> if f x (head y) == GT then [x] else y) []

-- 11.
myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy f = head . foldr (\x y ->
                                case y of
                                  [] -> [x]
                                  _ -> if f x (head y) == LT then [x] else y) []
