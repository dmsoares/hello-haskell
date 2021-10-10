-- |

module Ch10Lists where

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : []) = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- enumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool from to =
  go from to []
  where go f t r
          | f && t = reverse $ f : r
          | f > t || f = reverse r
          | otherwise = go (succ f) t $ f : r

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to =
  go from to []
  where go f t r
          | f == GT && t == GT = reverse $ GT : r
          | f > t || f == GT = reverse r
          | otherwise = go (succ f) t $ f : r

eftInt :: Int -> Int -> [Int]
eftInt =
  go []
  where go r f t
          | f > t = reverse r
          | otherwise = go (f : r) (succ f) t

eftChar :: Char -> Char -> [Char]
eftChar =
  go []
  where go r f t
          | f > t = reverse r
          | otherwise = go (f : r) (succ f) t


-- Exercises: Thy Fearful Symmetry
-- 1.
myWords :: String -> Char -> [String]
myWords s c =
  go s []
  where
    go :: String -> [String] -> [String]
    go string r
          | string == "" = reverse r
          | otherwise = go (drop 1 . dropWhile (/= c) $ string) $ takeWhile (/= c) string : r


-- Exercises: Comprehend Thy Lists
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

evenSqr = [x | x <- mySqr, even x]
mixSqr = [(x, y) | x <- mySqr, x < 50, y <- mySqr, y > 50]
takeCaps xs = [x | x <- xs, x `elem` ['A'..'Z']]
takeVowels xs = [x | x <- xs, x `elem` "aeiou"]

mySqrCubes = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]
mySqrCubesLen = length mySqrCubes

-- Exercises: Filtering
-- 1.
filterM3 :: Integral a => [a] -> [a]
filterM3 = filter (\x -> rem x 3 == 0)

-- 2.
lengthM3 :: Integral a => [a] -> Int
lengthM3 = length . filter (\x -> rem x 3 == 0)

-- 3.
remArticles :: String -> [String]
remArticles = filter pred . words
  where
    pred :: String -> Bool
    pred "a" = False
    pred "an" = False
    pred "the" = False
    pred _ = True

-- Exercises: Zipping
-- 1.
myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

-- 2.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

-- 3.
myZip' :: [a] -> [b] -> [(a, b)]
myZip' = myZipWith (,)
