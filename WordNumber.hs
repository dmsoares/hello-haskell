-- |

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
  case mod n 10 of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"

digits :: Int -> [Int]
digits n = go n []
  where
    go num nums
          | num == mod num 10 = mod num 10 : nums
          | otherwise = go (div num 10) $ mod num 10 : nums

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
