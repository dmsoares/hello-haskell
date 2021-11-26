-- |

module Palindrome where

import Data.Char
import Control.Monad
import System.Exit (exitSuccess)

strToLower :: String -> String
strToLower = map toLower

onlyLetters :: String -> String
onlyLetters = filter $ flip elem $ ['A'..'z'] ++ ['0'..'9']

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  if (strToLower . onlyLetters $ line1) ==
    (reverse . strToLower . onlyLetters $ line1)
    then putStrLn "It's a palindrome!"
    else exitSuccess
