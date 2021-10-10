-- |

module Ch10Lists where

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x : []) = Nothing
safeTail (_ : xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x

-- enumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool from to =
  go from to []
  where go f t r
          | f && t = reverse $ f : r
          | f > t || f = reverse r
          | otherwise = go (succ f) t $ f : r

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd from to =
  go from to []
  where go f t r
          | f == GT && t == GT = reverse $ GT : r
          | f > t || f == GT = reverse r
          | otherwise = go (succ f) t $ f : r

eftInt :: Int -> Int -> [Int]
eftInt =
  go []
  where go r f t
          | f > t = reverse r
          | otherwise = go (f : r) (succ f) t

eftChar :: Char -> Char -> [Char]
eftChar =
  go []
  where go r f t
          | f > t = reverse r
          | otherwise = go (f : r) (succ f) t
