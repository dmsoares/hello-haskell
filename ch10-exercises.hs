-- |

module CH10Exercises where

import Data.Time
import System.Posix (BaudRate(B1200))

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 50000))
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 1
  , DbNumber 5
  ]

-- Database
-- 1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate dbItems = [item | (DbDate item) <- dbItems]

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber dbItems = [item | (DbNumber item) <- dbItems]

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb dbItems = fromIntegral (sum dbNumbers) / fromIntegral (length dbNumbers)
  where dbNumbers = filterDbNumber dbItems

-- Scans exercises

fibs = 1 : scanl (+) 1 fibs

-- 1.
fibs20 = take 20 $ 1 : scanl (+) 1 fibs20

-- 2.
fibsLT100 = takeWhile (<100) $ 1 : scanl (+) 1 fibsLT100

-- 3.
factorial = 1 : scanl (*) 2 factorial
