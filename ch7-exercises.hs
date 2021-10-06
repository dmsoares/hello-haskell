-- |

module Ch7Exercises where

-- Grab Bag
-- 1.
mTha x y z = x * y * z

mThb x y = \z -> x * y * z

mThc x = \y -> \z -> x * y * z

mThd = \x -> \y -> \z -> x * y * z

-- 2.
-- d)

-- 3.
-- a)
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1

-- b)
addFive x y = (if x > y then y else x) + 5
addFiveLambda = \x -> \y -> ((if x > y then y else x) + 5)

-- c)
mflip f = \x -> \y -> f y x
mflip' f x y = f y x

-- Variety Pack
-- 1.
k :: (a, b) -> a
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

-- 2.
f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
-- f ((,,) a b c) ((,,) d e f) = (,) ((,) a d) ((,) c f)
f (a, b, c) (d, e, f) = ((a, d), (c, f))

-- Case Practice

functionC x y =
  case x > y of
    True -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0

-- Guard Duty
-- 1.
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9  = 'A'
  | y >= 0.8  = 'B'
  | y >= 0.7  = 'C'
  | y >= 0.59 = 'D'
  | y <  0.59 = 'F'
  where y = x / 100

-- 2.
pal xs
  | xs == reverse xs = True
  | otherwise        = False


-- 6.
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1


-- Let's write code
-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

-- a)
tensDigit' :: Integral a => a -> a
tensDigit' = snd . (`divMod` 10) . fst . (`divMod` 10)

-- c)
xDigit x = snd . (`divMod` 10) . fst . (`divMod` x)
hunsD = xDigit 100

-- 2.
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z =
  case z of
    False -> x
    True -> y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z
  | z = y
  | otherwise = x

-- 3.
g :: (a -> c) -> (a, b) -> (c, b)
g f (x, y) = (f x, y)
