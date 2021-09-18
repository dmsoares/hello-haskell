module Exercises where

-- ch.2 let and where
func1 = x * 3 + y
  where x = 3
        y = 1000

func2 = x * 5
  where y = 10
        x = 10 * 5 + y

func3 = z / x + y
  where x = 7
        y = negate x
        z = y * 10

-- ch.2 chapter exercises
-- -- parenthesization

-- 1.
func4 = 2 + (2 * 3) - 1
-- 2.
func5 = (^) 10 $ (1 + 1)
-- 3.
func6 = (2 ^ 2) * (4 ^ 5) + 1

-- More fun with functions

waxOn = x * 5
  where z = 7
        x = y ^ 2
        y = z + 8

triple x = x * 3

waxOff x = (*) (triple $ x + 10) 10

-- Scope
area d = pi * (r * r)
  where r = d / 2


-- Simple operations with text
-- 2.

func7 :: String -> String
func7 str = str ++ "!"

func8 :: String -> String
func8 str = [str !! 4]

func9 :: String -> String
func9 str = drop 9 str
