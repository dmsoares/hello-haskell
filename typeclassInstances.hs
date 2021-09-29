-- | Chapter 6

module Trivial where

data Trivial = Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

data Date =
  Date DayOfWeek Int deriving Show

instance Eq Date where
  (==) (Date weekday dayOfMonth)
        (Date weekday' dayOfMonth') =
    weekday == weekday'
    && dayOfMonth == dayOfMonth'
