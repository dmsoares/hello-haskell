module Ch17ApplicativeExercises where

import           Control.Applicative (liftA3)
import           Data.List           (elemIndex)

-- Lookups
-- 1.
added :: Maybe Integer
added =
  (+ 3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

w :: Maybe Int
w = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> w

-- 4.
xs = [1, 2, 3]

ys = [4, 5, 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y' :: Maybe Integer
y' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x <*> y)

-- Identity Instance
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

-- Constant Instance
newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap f (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant a) (Constant a') = Constant (a <> a')

-- Fixer Upper
-- 1.
x1 = const <$> Just "Hello" <*> pure "World"

-- 2.
x2 =
  (,,,) <$> Just 90
    <*> Just 10
    <*> Just "Tierness"
    <*> pure [1, 2, 3]
