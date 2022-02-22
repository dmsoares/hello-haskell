module Ch17ApplicativeExercises where

import           Control.Applicative
import           Data.List                (elemIndex)
import           Data.Monoid
import           Test.QuickCheck
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
