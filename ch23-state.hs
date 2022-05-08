module Ch23State where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
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

-- first, without State monad:
rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let (n0, s0) = randomR (1, 6) (mkStdGen 1)
      (n1, s1) = randomR (1, 6) s0
      (n2, _) = randomR (1, 6) s1
  (intToDie n0, intToDie n1, intToDie n2)

-- now, with State monad:
rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

--rollDie = state $ randomR (1, 6) >>= \(n, s) -> const (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

-- Keep on rolling
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
        let (die, nextGen) = randomR (1, 6) gen
         in go (sum + die) (count + 1) nextGen
