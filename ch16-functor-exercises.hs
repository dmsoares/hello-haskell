module Ch16FunctorExercises where

import           Test.QuickCheck
import           Test.QuickCheck.Function

-- Heavy Lifting
-- 1.
a = (+ 1) <$> read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c = fmap (* 2) (\x -> x - 2)

-- 4.
d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])

-- 5.
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = (read . ("123" ++)) . show <$> ioi
   in fmap (* 3) changed

-- Instances of Func
main :: IO ()
main = do
  quickCheck (functorCompose' :: IdentityFunctorCompose)
  quickCheck (functorCompose' :: PairFunctorCompose)
  quickCheck (functorCompose' :: TwoFunctorCompose)
  quickCheck (functorCompose' :: ThreeFunctorCompose)

functorCompose' ::
  (Eq (f c), Functor f) =>
  f a ->
  Fun a b ->
  Fun b c ->
  Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1.
newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

type IdentityFunctorCompose =
  Identity Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 2.
data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    Pair a <$> arbitrary

type PairFunctorCompose =
  Pair Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 3.
data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    Two a <$> arbitrary

type TwoFunctorCompose =
  Two Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 4.
data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three a b <$> arbitrary

type ThreeFunctorCompose =
  Three Int Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool
