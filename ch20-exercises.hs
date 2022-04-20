module Ch20Exercises where

import Data.Monoid
  ( Any (Any, getAny),
    Product (Product, getProduct),
    Sum (Sum, getSum),
  )

-- Library Functions
-- 1.
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . Prelude.foldMap Sum

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = getProduct . Prelude.foldMap Product

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . Prelude.foldMap (Any . (== a))

-- 4.
minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = foldr f Nothing
  where
    f a Nothing = Just a
    f a (Just a') = Just $ min a a'

-- 5.
maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = foldr f Nothing
  where
    f a Nothing = Just a
    f a (Just a') = Just $ max a a'

-- 6.
null :: Foldable t => t a -> Bool
null = foldr (const $ const False) True

-- 7.
length :: Foldable t => t a -> Int
length = foldr (const (+ 1)) 0

-- 8.
toList :: Foldable t => t a -> [a]
toList = foldr (:) []

-- 9.
fold :: (Foldable t, Monoid m) => t m -> m
fold = Prelude.foldMap id

-- 10.
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f = foldr ((<>) . f) mempty

-- Chapter Exercises
-- 1.
data Constant a b = Constant b
  deriving (Show)

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b

-- 2.
data Two a b = Two a b
  deriving (Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

-- 3.
data Three a b c = Three a b c
  deriving (Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- 4.
data Three' a b = Three' a b b
  deriving (Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = f b <> f b'

-- 5.
data Four a b = Four a b b b
  deriving (Show)

instance Foldable (Four a) where
  foldMap f (Four _ b b' b'') = f b <> f b' <> f b''

-- Filter function using foldMap
filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = Prelude.foldMap f
  where
    f a = if p a then pure a else mempty
