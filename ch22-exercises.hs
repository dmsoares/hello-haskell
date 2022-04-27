{-# LANGUAGE InstanceSigs #-}

module Ch22Exercises where

import Data.Char

-- Warming Up

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

tupled :: String -> (String, String)
tupled = (,) <$> cap <*> rev

tupled' :: String -> (String, String)
tupled' = (,) <$> rev <*> cap

tupledM :: String -> (String, String)
tupledM = do
  a <- cap
  b <- rev
  return (a, b)

tupledM' :: String -> (String, String)
tupledM' = cap >>= \c -> rev >>= \r x -> (c, r) -- or, '\r -> return (c, r)' instead of '\r x -> (c, r)'

-- Ask
newtype Reader r a = Reader {runReader :: r -> a}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

ask :: Reader a a
ask = Reader id

-- Reading Comprehension
-- 1. Write liftA2 yourself
myLiftA2 ::
  Applicative f =>
  (a -> b -> c) ->
  f a ->
  f b ->
  f c
myLiftA2 f x y = f <$> x <*> y

-- 2.
asks :: (r -> a) -> Reader r a
asks = Reader

-- 3. Implement Applicative for Reader
-- InstanceSigs enabled on top of file to allow type signatures in instance declarations
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ const a

  (<*>) ::
    Reader r (a -> b) ->
    Reader r a ->
    Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r $ ra r
