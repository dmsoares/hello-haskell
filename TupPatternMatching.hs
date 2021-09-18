-- |

module TupPatternMatching where

tupFunction :: (Int, [a]) -> (Int, [a]) -> (Int, [a])
tupFunction (a, b) (c, d) = (a + c, b ++ d)
