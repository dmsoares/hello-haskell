module Ch23State where

import Control.Applicative (liftA3)
import Control.Monad (mapM_, replicateM)
import Control.Monad.Trans.State
import qualified Data.DList as DL
import Data.Time (diffUTCTime)
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

-- FizzBuzz
-- typical solution
fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

main :: IO ()
main = do
  mapM_ (putStrLn . fizzBuzz) [1 .. 100]

-- solution using State
fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list =
  execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n =
  get >>= \s ->
    let result = fizzBuzz n : s
     in put result

main' :: IO ()
main' = do
  mapM_ putStrLn $
    reverse $ fizzbuzzList [1 .. 100]

-- solution using difference list instead of []
fizzbuzzList' :: [Integer] -> DL.DList String
fizzbuzzList' list =
  execState (mapM_ addResult' list) DL.empty

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main'' :: IO ()
main'' = do
  mapM_ putStrLn $ fizzbuzzList' [1 .. 100]
