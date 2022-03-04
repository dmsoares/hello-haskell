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

-- 2.
data PhhhbbtttEither b a
  = Left' a
  | Right' b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a)  = Left' $ f a
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (Left' a) <*> (Left' a') = Left' $ a a'
  (Right' b) <*> _         = Right' b
  _ <*> (Right' b)         = Right' b

instance Monad (PhhhbbtttEither b) where
  return = pure
  (Left' a) >>= k  = k a
  (Right' b) >>= k = Right' b

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (PhhhbbtttEither b a)
  where
  arbitrary =
    oneof [Left' <$> arbitrary, Right' <$> arbitrary]

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq
