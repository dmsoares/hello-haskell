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

-- Reader Monad
-- 1.
instance Monad (Reader r) where
  return = pure

  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = Reader $ \r -> runReader (aRb (ra r)) r

-- 2. Rewrite getDogRM
newtype HumanName
  = HumanName String
  deriving (Eq, Show)

newtype DogName
  = DogName String
  deriving (Eq, Show)

newtype Address
  = Address String
  deriving (Eq, Show)

data Person = Person
  { humanName :: HumanName,
    dogName :: DogName,
    address :: Address
  }
  deriving (Eq, Show)

data Dog = Dog
  { dogsName :: DogName,
    dogsAddress :: Address
  }
  deriving (Eq, Show)

-- sample data:
pers :: Person
pers =
  Person
    (HumanName "Big Bird")
    (DogName "Barkley")
    (Address "Sesame Street")

chris :: Person
chris =
  Person
    (HumanName "Chris Allen")
    (DogName "Papu")
    (Address "Austin")

getDogRM :: Reader Person Dog
getDogRM = do
  name <- Reader dogName
  addy <- Reader address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' =
  Reader dogName >>= \name ->
    Reader address >>= \addy ->
      return $ Dog name addy
