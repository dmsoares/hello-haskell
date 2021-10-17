-- |

module Ch9Exercises where

import Data.Char

-- 2.
filterUpper :: String -> String
filterUpper = filter isUpper

-- 3.
firstToUpper :: String -> String
firstToUpper [] = []
firstToUpper (x : xs) = toUpper x : xs

-- 4. using recursion and not map
allToUpper :: String -> String
allToUpper [] = []
allToUpper (x : xs) = toUpper x : allToUpper xs

-- 5.
take1toUpper :: String -> String
take1toUpper = (: []) . toUpper . head

-- Ciphers
cCipher :: Int -> String -> String
cCipher n = map $ rot n
  where
    rot :: Int -> Char -> Char
    rot n c = chr $ mod (n + ord c) 144697

cUncipher :: Int -> String -> String
cUncipher n = map $ rot n
  where
    rot :: Int -> Char -> Char
    rot n c = chr $ mod (ord c - n) 144697

-- Writing standard functions
-- 1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x : xs) = x || myOr xs

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x : xs) = f x || myAny f xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f = myOr . map f

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem y (x : xs) = (y == x) || myElem y xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' n = any (== n)

-- 4.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- 5. same as concat
squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

-- 6. same as concatMap
squishMap :: (a-> [b]) -> [a] -> [b]
squishMap f = squish . map f

-- 7. same as squish but using squishMap
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- First take:
-- myMaximumBy f (x : xs) =
--   go f x xs
--   where
--     go _ x [] = x
--     go f y (x : xs)
--       | f y x == GT = go f y xs
--       | otherwise = go f x xs

-- Second take:
myMaximumBy _ [] = error "myMaximumBy: empty list"
myMaximumBy f (x : (y : ys))
  | null ys = if f x y == GT then x else y
  | f x y == GT = myMaximumBy f $ x : ys
  | otherwise = myMaximumBy f $ y : ys


-- 9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "myMinimumBy: empty list"
myMinimumBy f (x : (y : ys))
  | null ys = if f x y == GT then x else y
  | f x y == GT = myMinimumBy f $ x : ys
  | otherwise = myMinimumBy f $ y : ys

-- 10.
myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare
