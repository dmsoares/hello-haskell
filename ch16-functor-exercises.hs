{-# LANGUAGE FlexibleInstances #-}

module Ch16FunctorExercises where

import           GHC.Arr
import           Test.QuickCheck
import           Test.QuickCheck.Function

-- Heavy Lifting
-- 1.
a :: [Int]
a = (+ 1) <$> read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c :: Integer -> Integer
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
  quickCheck (functorCompose' :: Three'FunctorCompose)
  quickCheck (functorCompose' :: FourFunctorCompose)
  quickCheck (functorCompose' :: Four'FunctorCompose)
  putStrLn "Chapter exercises:"
  quickCheck (functorCompose' :: BASEFunctorCompose)
  quickCheck (functorCompose' :: BAMSEFunctorCompose)
  quickCheck (functorCompose' :: QuantFunctorCompose)
  quickCheck (functorCompose' :: KFunctorCompose)
  quickCheck (functorCompose' :: FlipFunctorCompose)
  quickCheck (functorCompose' :: GoateeFunctorCompose)
  quickCheck (functorCompose' :: LiftFunctorCompose)
  quickCheck (functorCompose' :: ParappaFunctorCompose)
  quickCheck (functorCompose' :: IgnoreOneFunctorCompose)
  quickCheck (functorCompose' :: NotoriousFunctorCompose)
  quickCheck (functorCompose' :: ListFunctorCompose)

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

-- 5.
data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    Three' a b <$> arbitrary

type Three'FunctorCompose =
  Three' Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 6.
data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    Four a b c <$> arbitrary

type FourFunctorCompose =
  Four Int Int Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 7.
data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    Four' a a' a'' <$> arbitrary

type Four'FunctorCompose =
  Four' Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 8.
-- Can't implement instance of Functor for Trivial. It has kind *.

-- Possibly
data Possibly a
  = LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap f (Yeppers a) = Yeppers $ f a
  fmap f LolNope     = LolNope

-- Short Exercise on Either
-- 1.
data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap f (First a)  = First a

-- Chapter exercises
-- 1. no can do
-- 2.
data BoolAndSomethingElse a
  = False' a
  | True' a
  deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a)  = True' (f a)

instance Arbitrary a => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = oneof [False' <$> arbitrary, True' <$> arbitrary]

type BASEFunctorCompose =
  BoolAndSomethingElse Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 3.
data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a
  deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish    = Falsish
  fmap f (Truish a) = Truish (f a)

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = oneof [return Falsish, Truish <$> arbitrary]

type BAMSEFunctorCompose =
  BoolAndMaybeSomethingElse Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 4.
newtype Mu f = InF {outF :: f (Mu f)} -- not possible, has kind: (* -> *) -> *

-- 5.
data D
  = D (Array Word Word) Int Int -- not possible, has kind: *

-- Rearrange the arguments to the type constructor
-- of the datatype so the Functor instance works.
-- 1.
data Sum' b a
  = First' a
  | Second' b

instance Functor (Sum' a) where
  fmap f (First' a)  = First' (f a)
  fmap f (Second' b) = Second' b

-- 2.
data Company a c b
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b)  = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.
-- 1.
data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap f (Bloor b) = Bloor (f b)
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary =
    oneof [return Finance, Desk <$> arbitrary, Bloor <$> arbitrary]

type QuantFunctorCompose =
  Quant Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 2.
data K a b
  = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a

instance Arbitrary a => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

type KFunctorCompose =
  K Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 3.
newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

newtype K' a b
  = K' a
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (K' a b) where
  arbitrary = K' <$> arbitrary

instance Functor (Flip K' a) where
  fmap f (Flip (K' b)) = Flip (K' (f b))

instance Arbitrary b => Arbitrary (Flip K' a b) where
  arbitrary = Flip <$> arbitrary

type FlipFunctorCompose =
  Flip K' Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 4.
data EvilGoateeConst a b
  = GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

type GoateeFunctorCompose =
  EvilGoateeConst Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 5.
data LiftItOut f a
  = LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)

instance (Arbitrary a) => Arbitrary (LiftItOut Maybe a) where
  arbitrary =
    oneof [return $ LiftItOut Nothing, LiftItOut . Just <$> arbitrary]

type LiftFunctorCompose =
  LiftItOut Maybe Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 6.
data Parappa f g a
  = DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (f <$> fa) (f <$> ga)

instance Arbitrary a => Arbitrary (Parappa Maybe [] a) where
  arbitrary = do
    oneof
      [ (\a -> DaWrappa Nothing [a]) <$> arbitrary,
        (\a -> DaWrappa (Just a) [a]) <$> arbitrary
      ]

type ParappaFunctorCompose =
  Parappa Maybe [] Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 7.
data IgnoreOne f g a b
  = IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (f <$> gb)

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne [] [] a b) where
  arbitrary = do
    a <- arbitrary
    IgnoringSomething a . (: []) <$> arbitrary

type IgnoreOneFunctorCompose =
  IgnoreOne [] [] Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 8.
data Notorious g o a t
  = Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (f <$> gt)

instance (Arbitrary o, Arbitrary a, Arbitrary t) => Arbitrary (Notorious TalkToMe o a t) where
  arbitrary = do
    o <- arbitrary
    a <- arbitrary
    t <- arbitrary
    s <- arbitrary
    return $ Notorious (Print s o) (Print s a) (Print s t)

type NotoriousFunctorCompose =
  Notorious TalkToMe Int Int Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 9.
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil         = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    oneof [return Nil, return $ Cons a Nil]

type ListFunctorCompose =
  List Int ->
  Fun Int Int ->
  Fun Int Int ->
  Bool

-- 10.
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats
      (GoatLord a)
      (GoatLord a)
      (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap f NoGoat            = NoGoat
  fmap f (OneGoat a)       = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (f <$> a) (f <$> a) (f <$> a)

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    oneof
      [ return NoGoat,
        return $ OneGoat a
      ]

-- 11.
data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Show a => Show (TalkToMe a) where
  show Halt        = "TalkToMe: Halt"
  show (Print s a) = "TalkToMe: " ++ s ++ " " ++ show a
  show _           = "TalkToMe: Read fn"

instance Eq a => Eq (TalkToMe a) where
  (==) Halt Halt                 = True
  (==) (Print s a) (Print s' a') = s == s' && a == a'
  (==) _ _                       = False

instance Functor TalkToMe where
  fmap f Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g)    = Read (f . g)
