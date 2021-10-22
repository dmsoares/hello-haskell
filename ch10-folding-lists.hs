-- |

module CH10FoldingList where

fr :: (a -> b -> b) -> b -> [a] -> b
fr _ z [] = z
fr f z (x:xs) = f x (fr f z xs)

y = fr (flip const) 'a' [1..5]
