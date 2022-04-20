module Ch21Exercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = do
  quickBatch $ traversable (undefined :: Identity (Maybe Int, Maybe Int, Int, String))
  quickBatch $
    traversable (undefined :: Constant String (Maybe String, Maybe String, String, String))
  quickBatch $
    traversable (undefined :: Optional (Maybe String, Maybe String, String, String))

-- Traversable instances
-- Identity
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)

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
