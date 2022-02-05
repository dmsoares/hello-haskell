module Functor where

import           Test.QuickCheck
import           Test.QuickCheck.Function

-- P-Funkshun

main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)

  putStr "liftedReplace lms: "
  print (liftedReplace lms)

  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)

  putStr "twiceLifted lms: "
  print (twiceLifted lms)

  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)

  putStr "thriceLifted lms: "
  print (thriceLifted lms)

  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

-- Functor laws
-- fmap id = id
-- fmap (f . g) = (fmap f) . (fmap g)

-- QuickCheck properties
functorIdentity ::
  (Functor f, Eq (f a)) =>
  f a ->
  Bool
functorIdentity f =
  fmap id f == f

functorCompose ::
  (Functor f, Eq (f c)) =>
  (a -> b) ->
  (b -> c) ->
  f a ->
  Bool
functorCompose f g x =
  (fmap (g . f) x) == ((fmap g) . (fmap f)) x

-- Making QuickCheck generate functions
functorCompose' ::
  (Eq (f c), Functor f) =>
  f a ->
  Fun a b ->
  Fun b c ->
  Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)
