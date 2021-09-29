{-# LANGUAGE NoMonomorphismRestriction  #-}

module DetermineTheType where

-- 1.
-- Determine value returned and its type

-- a = (* 9) 6 -- Num a => a
-- b = head [(0,"doge"),(1,"kitteh")] -- (Num a) => (a, [Char])
-- c =  head [(0 :: Integer , "doge"),(1,"kitteh")] -- (Integer, [Char])
-- d = if False then True else False -- Bool
-- e = length [1, 2, 3, 4, 5] -- Int
-- f = (length [1, 2, 3, 4]) > (length "TACOAT") -- Bool

-- 2.
-- Type of w?

-- x = 5
-- y = x + 5
-- w = y * 10 -- Num a => a

-- 3.
-- Type of z?

-- z y = y * 10 -- Num a => a -> a

-- 4.
-- Type of f?

-- f = 4 / y -- Fractional a => a

-- 5.
-- Type of h?

-- x = "Julie"
-- y = " <3 "
-- z = "Haskell"
-- h = x ++ y ++ z -- [Char]

-- Does it compile?
-- 1.

-- bigNum = (^) 5
-- wahoo = bigNum $ 10

-- 2.

-- x = print
-- y = print "woohoo!"
-- z = x "hello world"

-- 3.

-- a = (+)
-- b = 5
-- c = a 10
-- d = c 200

-- 4.

-- a = 12 + b
-- b = 10000 * c

-- Fix it
-- 1.

fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x < y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"

-- 2.
