{-# LANGUAGE FlexibleInstances #-}
-- |

module Newtype where

class TooMany a where
  tooMany :: a -> Bool

-- instance TooMany (Int, String) where
  -- tooMany (x, y) = x > 42

newtype Tuple = Tuple (Int, String)

instance TooMany Tuple where
  tooMany (Tuple (x, y)) = x > 42

instance TooMany (Int, Int) where
  tooMany (x, y) = (>42) $ x + y
