module Ch16FunctorExercises where

-- Heavy Lifting
-- 1.
a = (+ 1) <$> read "[1]" :: [Int]

-- 2.
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3.
c = fmap (* 2) (\x -> x - 2)

-- 4.
d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])

-- 5.
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = (read . ("123" ++)) . show <$> ioi
   in fmap (* 3) changed
