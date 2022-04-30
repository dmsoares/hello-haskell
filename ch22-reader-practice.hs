module ReaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]

y = [4, 5, 6]

z = [7, 8, 9]

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
-- this one will return Nothing
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

-- make Maybe (,) using applicative
-- tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- tuple of two applications of z'
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

-- some helper functions
summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- lifts a boolean function over two partially applied functions
bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' :: Maybe Integer
s' = summed <$> ((,) <$> xs <*> ys)

-- main exercise
main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(> 3), (< 8), even] 7
  print $ and $ sequA 4
  print $ fromMaybe [] $ sequA <$> s'
  print $ fromMaybe False $ bolt <$> ys
  -- the same as above
  print $ maybe [] sequA s'
  print $ maybe False bolt ys

-- see Shawty for implementation of ReaderT
