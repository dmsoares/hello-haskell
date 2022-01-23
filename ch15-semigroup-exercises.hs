module Ch15SemigroupExercises where

import           Data.IntMap     (compose)
import           Data.Semigroup  (Product, Semigroup)
import           Test.QuickCheck

-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc ::
  (Eq m, Semigroup m) =>
  m ->
  m ->
  m ->
  Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentAssoc String)
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (semigroupAssoc :: ThreeAssoc String String String)
  quickCheck (semigroupAssoc :: FourAssoc String String String String)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc String String)
  quickCheck (combSemigroupAssoc :: CombAssoc Int (Product Int))
  quickCheck (compSemigroupAssoc :: CompAssoc Int)

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup (Identity a) where
  (Identity a) <> _ = Identity a

instance
  Arbitrary a =>
  Arbitrary (Identity a)
  where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentAssoc a =
  Identity a -> Identity a -> Identity a -> Bool

-- 3.
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance
  (Arbitrary a, Arbitrary b) =>
  Arbitrary (Two a b)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc a b =
  Two a b -> Two a b -> Two a b -> Bool

-- 4.
data Three a b c = Three a b c deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c)
  where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance
  ( Arbitrary a,
    Arbitrary b,
    Arbitrary c
  ) =>
  Arbitrary (Three a b c)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc a b c =
  Three a b c -> Three a b c -> Three a b c -> Bool

-- 5.
data Four a b c d = Four a b c d deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d)
  where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance
  ( Arbitrary a,
    Arbitrary b,
    Arbitrary c,
    Arbitrary d
  ) =>
  Arbitrary (Four a b c d)
  where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc a b c d =
  Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> b  = b
  (BoolConj False) <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    b <- arbitrary
    return $ BoolConj b

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _  = BoolDisj True
  (BoolDisj False) <> b = b

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.
data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  (Fst a) <> o = o

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency
      [ (1, return (Fst a)),
        (1, return (Snd b))
      ]

type OrAssoc a b =
  Or a b -> Or a b -> Or a b -> Bool

-- 9.
newtype Combine a b = Combine {unCombine :: (a -> b)}

instance Show (Combine a b) where
  show (Combine _) = "Combine"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\n -> f n <> g n)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    aToB <- arbitrary
    return $ Combine aToB

combSemigroupAssoc ::
  (Eq b, Semigroup b) =>
  Combine a b ->
  Combine a b ->
  Combine a b ->
  a ->
  Bool
combSemigroupAssoc f g h a =
  unCombine (f <> g <> h) a
    == unCombine (f <> (g <> h)) a

type CombAssoc a b =
  Combine a b -> Combine a b -> Combine a b -> a -> Bool

-- 10.
newtype Comp a = Comp {unComp :: (a -> a)}

instance Show (Comp a) where
  show comp = "Compose"

instance Semigroup (Comp a) where
  (Comp a) <> (Comp a') = Comp (a . a')

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    aToA <- arbitrary
    return $ Comp aToA

compSemigroupAssoc ::
  Eq a => Comp a -> Comp a -> Comp a -> a -> Bool
compSemigroupAssoc f g h a =
  unComp (f <> g <> h) a == unComp (f <> (g <> h)) a

type CompAssoc a =
  Comp a -> Comp a -> Comp a -> a -> Bool
