{-# LANGUAGE FlexibleContexts #-}

module Ch21Exercises where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Traversable instances - RUN MAIN FOR TESTS -
-- Identity
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Constant
newtype Constant a b = Constant {getConstant :: a}
  deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Foldable (Constant a) where
  foldMap _ x = mempty

instance Monoid a => Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

-- Maybe
data Optional a
  = Nada
  | Yep a
  deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep $ f a

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = Yep <$> arbitrary

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- List
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> xs = xs
  xs <> Nil = xs
  Cons x xs <> ys =
    case xs of
      Nil -> Cons x ys
      _ -> Cons x (xs <> ys)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) $ fmap f xs

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = f x <> foldMap f xs

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [(1, return Nil), (3, liftA2 Cons arbitrary arbitrary)]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Three
data Three a b c
  = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

instance
  (Arbitrary a, Arbitrary b, Arbitrary c) =>
  Arbitrary (Three a b c)
  where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Pair
data Pair a b
  = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a $ f b

instance Foldable (Pair a) where
  foldMap f (Pair _ b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

-- Big
data Big a b
  = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')

instance Foldable (Big a) where
  foldMap f (Big _ b b') = f b <> f b'

instance Traversable (Big a) where
  traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

-- Bigger
data Bigger a b
  = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b b' b'') = f b <> f b' <> f b''

instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = liftA3 (Bigger a) (f b) (f b') (f b'')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = liftA3 Bigger arbitrary arbitrary arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (f <$> na) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

instance
  ( Functor n,
    Arbitrary (n a),
    Arbitrary a
  ) =>
  Arbitrary (S n a)
  where
  arbitrary =
    S <$> arbitrary <*> arbitrary

instance
  ( Applicative n,
    Testable (n Property),
    EqProp a
  ) =>
  EqProp (S n a)
  where
  (S x y) =-= (S p q) =
    property ((=-=) <$> x <*> p) .&. (y =-= q)

main :: IO ()
main =
  hspec $ do
    describe "Identity" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: Identity (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: Identity (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: Identity (Maybe Int, Maybe Int, Int, String))

    describe "Constant" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: Constant String (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: Constant String (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: Constant String (Maybe String, Maybe String, String, String))

    describe "Optional" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: Optional (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: Optional (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: Optional (Maybe String, Maybe String, String, String))

    describe "List" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: List (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: List (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: List (Maybe String, Maybe String, String, String))

    describe "Three" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: Three String String (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: Three String String (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: Three String String (Maybe String, Maybe String, String, String))

    describe "Pair" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: Pair String (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: Pair String (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: Pair String (Maybe String, Maybe String, String, String))

    describe "Big" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: Big String (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: Big String (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: Big String (Maybe String, Maybe String, String, String))

    describe "Bigger" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: Bigger String (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: Bigger String (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: Bigger String (Maybe String, Maybe String, String, String))

    describe "S" $ do
      it "Functor" $
        property $ quickBatch $ functor (undefined :: S Maybe (Int, Float, String))
      it "Foldable" $
        property $ quickBatch $ foldable (undefined :: S Maybe (Int, Float, String, Int, String))
      it "Traversable" $
        property $ quickBatch $ traversable (undefined :: S Maybe (Maybe String, Maybe String, String, String))
