module Ch15MonoidExercises where

import           Test.QuickCheck           (Arbitrary, quickCheck)
import           Test.QuickCheck.Arbitrary

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)

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
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool
