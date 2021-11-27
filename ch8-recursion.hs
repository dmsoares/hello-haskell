-- |

module Recursion where

-- factorial
f :: Integer -> Integer
f 0 = 1
f x = x * f (x - 1)

-- on the way to recursion

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + incTimes (times - 1) n

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f $ applyTimes (n - 1) f b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' n = applyTimes n (+1)

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes' (n - 1) f $ b

-- Bottom

g :: Bool -> Int
g True = error "blah"
g False = 0

h :: Bool -> Int
h False = 0

h' :: Bool -> Maybe Int
h' False = Just 0
h' _ = Nothing

-- Fibonacci numbers

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

-- Integral division

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer

d :: Numerator
  -> Denominator
  -> Quotient
d num den
  | num < den = 0
  | otherwise = (+1) . d (num - den) $ den


-- Exercises

-- Recursion
-- 2.
mySum :: (Eq a, Num a) => a -> a
mySum 0 = 0
mySum n = n + mySum (n - 1)

-- 3.
m :: (Integral a) => a -> a -> a
m n1 n2 = go n1 n2 0
  where go x y r
          | x == 0 = r
          | otherwise = go (x - 1) y (r + y)

multiply :: (Eq a, Num a) => a -> a -> a
multiply 0 _ = 0
multiply _ 0 = 0
multiply x y = y + multiply (x - 1) y

-- Fixing dividedBy
dividedBy :: (Integral a) => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

data DividedResult = Result (Integer, Integer) | DividedByZero deriving Show

dividedBy' :: Integer -> Integer -> DividedResult
dividedBy' _ 0 = DividedByZero
dividedBy' num denom = go num denom 0 (num * denom < 0)
  where go n d count invert
          | abs n < abs d = if invert then Result (-count, n) else Result (count, n)
          | otherwise = go (abs n - abs d) d (count + 1) invert

-- McCarthy 91 function

mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91(mc91 (n + 11))
