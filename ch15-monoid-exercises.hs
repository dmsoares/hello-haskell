module Ch15MonoidExercises where

import           Test.QuickCheck           (Arbitrary, quickCheck)
import           Test.QuickCheck.Arbitrary

main :: IO ()
main = do
  quickCheck (monoidAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidAssoc :: IdentAssoc String)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  quickCheck (monoidAssoc :: TwoAssoc String String)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  quickCheck (monoidAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

monoidAssoc ::
  (Eq m, Monoid m) =>
  m ->
  m ->
  m ->
  Bool
monoidAssoc a b c =
  (a `mappend` (b `mappend` c)) == ((a `mappend` b) `mappend` c)

monoidLeftIdentity ::
  (Eq m, Monoid m) =>
  m ->
  Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity ::
  (Eq m, Monoid m) =>
  m ->
  Bool
monoidRightIdentity a = (a `mappend` mempty) == a

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

-- 2.
newtype Identity a
  = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= return . Identity

type IdentAssoc a =
  Identity a -> Identity a -> Identity a -> Bool

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = arbitrary >>= \a -> arbitrary >>= (return . Two a)

type TwoAssoc a b =
  Two a b -> Two a b -> Two a b -> Bool

-- 4.
newtype BoolConj
  = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> b  = b
  (BoolConj False) <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool
