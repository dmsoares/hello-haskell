-- |

module Reverse where

import Distribution.Utils.Structured (structuredEncode)
rvrs :: String -> String
rvrs str = first ++ " " ++ second ++ " " ++ third
  where first = drop 9 str
        second = drop 6 $ take 8 str
        third = take 5 str

main :: IO ()
main = print $ rvrs "Curry is awesome"
