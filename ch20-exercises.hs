module Ch20Exercises where

import Data.Monoid
  ( Any (Any, getAny),
    Product (Product, getProduct),
    Sum (Sum, getSum),
  )

-- Library Functions
-- 1.
sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

-- 2.
product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

-- 3.
elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = getAny . foldMap (Any . (== a))

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
fold = foldMap id
