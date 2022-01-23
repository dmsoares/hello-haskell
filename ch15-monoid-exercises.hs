module Ch15MonoidExercises where

import           Test.QuickCheck           (Arbitrary, quickCheck)
import           Test.QuickCheck.Arbitrary

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (semigroupAssoc :: IdentAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

semigroupAssoc ::
  (Eq m, Semigroup m) =>
  m ->
  m ->
  m ->
  Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity ::
  (Eq m, Monoid m) =>
  m ->
  Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity ::
  (Eq m, Monoid m) =>
  m ->
  Bool
monoidRightIdentity a = (a <> mempty) == a

-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

-- 2.
newtype Identity a
  = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

type IdentAssoc a =
  Identity a -> Identity a -> Identity a -> Bool
