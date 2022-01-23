module Ch15Exercises where

import           Control.Monad
import           Data.Monoid
import           Test.QuickCheck

-- Optional Monoid
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a
  => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada          = Nada
  Nada <> o             = o
  o <> Nada             = o
  (Only a) <> (Only a') = Only (a <> a')

-- Madness
type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e <> "! he said " <>
  adv <> " as he jumped into his car " <>
  noun <> " and drove off with his " <>
  adj <> " wife"

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
  mconcat [ e
          , "! he said "
          , adv
          , " as he jumped into his car "
          , noun
          , " and drove off with his "
          , adj
          , " wife" ]

-- Maybe Another Monoid
monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m
                    -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (First' (Only a)) <> _  = First' (Only a)
  _ <> (First' (Only b))  = First' (Only b)
  _ <> _                  = First' Nada

instance Monoid a => Monoid (First' a) where
  mempty = First' Nada
  mappend = (<>)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, return (First' (Only a)))
              , (1, return (First' Nada)) ]


firstMappend :: Monoid a => First' a
                         -> First' a
                         -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
