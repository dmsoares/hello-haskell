{-# LANGUAGE InstanceSigs #-}

module Ch23Exercises where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import GhcPlugins (notNull)
import System.Random

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
    1 -> DieOne
    2 -> DieTwo
    3 -> DieThree
    4 -> DieFour
    5 -> DieFive
    6 -> DieSix
    x ->
      error $ -- using error like this is a terrible idea: don't do it!
        "intToDie got non 1-6 integer: "
          ++ show x

-- Roll Your Own
-- 1.
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= n = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go
              (sum + die)
              (count + 1)
              nextGen

-- 2.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 [] g
  where
    go :: Int -> [Die] -> StdGen -> (Int, [Die])
    go sum die gen
      | sum >= n = (length die, die)
      | otherwise =
        let (newDie, nextGen) = randomR (1, 6) gen
         in go
              (sum + newDie)
              (die ++ [intToDie newDie])
              nextGen

-- Write State for yourself
rollDie' :: Moi StdGen Die
rollDie' = intToDie <$> Moi (randomR (1, 6))

rollDieThreeTimes :: Moi StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie' rollDie' rollDie'

newtype Moi s a = Moi {runMoi :: s -> (a, s)}

-- State Functor
instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) =
    Moi $ \x ->
      let (a, s) = g x
       in (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ (,) a -- s -> (a, s) ~ (a,)

  (<*>) ::
    Moi s (a -> b) ->
    Moi s a ->
    Moi s b
  (Moi f) <*> (Moi g) =
    Moi $ \s ->
      let (a1, s1) = f s
          (a2, s2) = g s1
       in (a1 a2, s2)

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= k =
    Moi $ \s ->
      let (a1, s') = f s
       in runMoi (k a1) s'
