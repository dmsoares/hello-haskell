{-# LANGUAGE Rank2Types #-}

module Ch17ApplicativeExercises where

import           Control.Applicative      (liftA3)
import           Data.List                (elemIndex)
import           Data.Monoid
import           Test.QuickCheck          hiding (Failure, Success)
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

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

--
v =
  (.)
    <$> [(+ 1)]
    <*> [(* 2)]
    <*> [1, 2, 3]

v' = [(+ 1)] <*> ([(* 2)] <*> [1, 2, 3])

-- List Applicative Exercise
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = append (f <$> xs) (fs <*> xs)
    where
      append Nil list         = list
      append (Cons x xs) list = Cons x (append xs list)

instance Arbitrary a => Arbitrary (List a) where -- from stackoverflow
  arbitrary = sized go
    where
      go 0 = pure Nil
      go n = do
        xs <- go (n - 1)
        x <- arbitrary
        return (Cons x xs)

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- flatMap
append :: List a -> List a -> List a
append Nil list         = list
append (Cons x xs) list = Cons x (append xs list)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil        = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f Nil         = Nil
flatMap f (Cons a as) = append (f a) (flatMap f as)

-- ZipList Applicative Exercise
take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons a as) = Cons a (take' (n - 1) as)

newtype ZipList' a
  = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where
      xs' =
        let (ZipList' l) = xs
         in take' 3000 l
      ys' =
        let (ZipList' l) = ys
         in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' $ go x
    where
      go x = Cons x (go x)

  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' fList) (ZipList' xList) =
    ZipList' $ go fList xList
    where
      go Nil _ = Nil
      go _ Nil = Nil
      go (Cons f fs) (Cons x xs) =
        Cons (f x) (go fs xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

-- Variations on Either
data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success a)  = Success (f a)
  (<*>) (Failure e) (Failure e') = Failure (e <> e')
  (<*>) (Failure e) _            = Failure e
  (<*>) _ (Failure e)            = Failure e

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary =
    oneof [Failure <$> arbitrary, Success <$> arbitrary]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

-- Chapter Exercises
-- Specialization
-- 1. []
listPure :: a -> [a]
listPure = pure

listAp :: [a -> b] -> [a] -> [b]
listAp = (<*>)

-- 2. IO
ioPure :: a -> IO a
ioPure = pure

ioAp :: IO (a -> b) -> IO a -> IO b
ioAp = (<*>)

-- 3. (,) a
tuplePure :: Monoid a => b -> (a, b)
tuplePure = pure

tupleAp :: Monoid a => (a, b -> c) -> (a, b) -> (a, c)
tupleAp = (<*>)

-- 4. (->) e
fnPure :: a -> (->) e a
fnPure = pure

fnAp :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
fnAp = (<*>)

-- 1.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    Pair a <$> arbitrary

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)

-- 4.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' a f f') (Three' a' b b') = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three' a b <$> arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four a b c f) (Four a' b' c' d) =
    Four (a <> a') (b <> b') (c <> c') (f d)

-- 6.
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a0 a1 a2 b) = Four' a0 a1 a2 (f b)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' a0 a1 a2 f) (Four' a0' a1' a2' b) =
    Four' (a0 <> a0') (a1 <> a1') (a2 <> a2') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a0 <- arbitrary
    a1 <- arbitrary
    a2 <- arbitrary
    Four' a0 a1 a2 <$> arbitrary

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
