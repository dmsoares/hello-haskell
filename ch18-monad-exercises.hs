module Ch18MonadExercises where

import           Control.Monad
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

-- Create bind in terms of fmap and join
bind :: Monad m => (a -> m b) -> m a -> m b
bind f = join . fmap f

bind' :: [a] -> (a -> [b]) -> [b]
bind' = flip concatMap -- concatMap is (>>=) specialized to lists!!

-- Either Monad
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a)  = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  (First a) <*> _           = First a
  _ <*> (First a)           = First a
  (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
  return = pure
  (First a) >>= _  = First a
  (Second b) >>= k = k b

-- Chapter Exercises
-- 1.
data Nope a
  = NopeDotJpg
  deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq
