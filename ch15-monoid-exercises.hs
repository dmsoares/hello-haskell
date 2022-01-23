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
  quickCheck (monoidAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  quickCheck (monoidCombAssoc :: CombAssoc String String)
  quickCheck (monoidCombLeftIdentity :: Combine String String -> String -> Bool)
  quickCheck (monoidCombRightIdentity :: Combine String String -> String -> Bool)
  quickCheck (monoidCompAssoc :: CompAssoc String)
  quickCheck (monoidCompLeftIdentity :: Comp String -> String -> Bool)
  quickCheck (monoidCompRightIdentity :: Comp String -> String -> Bool)

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

-- 5.
newtype BoolDisj
  = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj True) <> _  = BoolDisj True
  (BoolDisj False) <> b = b

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    b <- arbitrary
    return $ BoolDisj b

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 6.
newtype Combine a b = Combine {unCombine :: (a -> b)}

instance Show (Combine a b) where
  show _ = "Combine"

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (\a -> f a <> g a)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (const mempty)
  mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CombAssoc a b =
  Combine a b -> Combine a b -> Combine a b -> a -> Bool

monoidCombAssoc ::
  (Eq b, Monoid b) =>
  Combine a b ->
  Combine a b ->
  Combine a b ->
  a ->
  Bool
monoidCombAssoc f g h a =
  unCombine (f <> g <> h) a == unCombine (f <> (g <> h)) a

monoidCombLeftIdentity ::
  (Eq b, Monoid b) =>
  Combine a b ->
  a ->
  Bool
monoidCombLeftIdentity (Combine f) a =
  unCombine (mempty `mappend` (Combine f)) a == unCombine (Combine f) a

monoidCombRightIdentity ::
  (Eq b, Monoid b) =>
  Combine a b ->
  a ->
  Bool
monoidCombRightIdentity (Combine f) a =
  unCombine ((Combine f) `mappend` mempty) a == unCombine (Combine f) a

-- 7.
newtype Comp a = Comp {unCompose :: a -> a}

instance Show (Comp a) where
  show _ = "Comp"

instance Semigroup (Comp a) where
  (Comp f) <> (Comp g) = Comp (f . g)

instance Monoid (Comp a) where
  mempty = Comp id
  mappend = (<>)

instance
  (CoArbitrary a, Arbitrary a) =>
  Arbitrary (Comp a)
  where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

monoidCompAssoc ::
  Eq a =>
  Comp a ->
  Comp a ->
  Comp a ->
  a ->
  Bool
monoidCompAssoc f g h a =
  unCompose (f <> g <> h) a == unCompose (f <> (g <> h)) a

type CompAssoc a =
  Comp a -> Comp a -> Comp a -> a -> Bool

monoidCompLeftIdentity ::
  Eq a => Comp a -> a -> Bool
monoidCompLeftIdentity c a =
  unCompose (mempty <> c) a == unCompose c a

monoidCompRightIdentity ::
  Eq a => Comp a -> a -> Bool
monoidCompRightIdentity c a =
  unCompose (c <> mempty) a == unCompose c a
