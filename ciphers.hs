module Ciphers where

import Data.Char

cipher :: IO ()
cipher = do
  putStrLn "Insert message:"
  msg <- getLine
  putStrLn "Insert cipher:"
  cphr <- getLine
  putStrLn $ cipher' msg cphr

decipher :: IO ()
decipher = do
  putStrLn "Insert message:"
  msg <- getLine
  putStrLn "Insert cipher:"
  cphr <- getLine
  putStrLn $ decipher' msg cphr

cipher' :: String -> String -> String
cipher' xs w =
  go xs w f 0 where
  go [] _ _ _ = []
  go (x:xs) w f c = f x w c : go xs w f (c + 1)
  f x w c = chr $ ord x + (ord (toUpper $ w !! mod c (length w)) - ord 'A')

decipher' :: String -> String -> String
decipher' xs w =
  go xs w f 0 where
  go [] _ _ _ = []
  go (x:xs) w f c = f x w c : go xs w f (c + 1)
  f x w c = chr $ ord x - (ord (toUpper $ w !! mod c (length w)) - ord 'A')
