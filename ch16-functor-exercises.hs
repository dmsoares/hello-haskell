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
