module Listy where

instance Semigroup a => Semigroup (Listy a) where
  (<>) (Listy l) (Listy l') =
    Listy $ mappend l l'

instance Monoid a => Monoid (Listy a) where
  mempty = Listy []

newtype Listy a =
  Listy [a]
  deriving (Eq, Show)

