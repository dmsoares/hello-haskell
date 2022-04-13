module Ch20Exercises where

import Data.Foldable
import Data.Monoid

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
