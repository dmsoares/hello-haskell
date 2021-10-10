-- |

module PoemLines where

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
          ++ thirdSen ++ fourthSen

myLines :: String -> Char -> [String]
myLines sentences char =
  go sentences []
  where
    go s r
      | s == "" = reverse r
      | otherwise = go (drop 1 . dropWhile (/= char) $ s) $ takeWhile (/= char) s : r

shouldEqual =
  [ "Tyger Tyger, burning bright\n"
  , "In the forests of the night\n"
  , "What immortal hand or eye\n"
  , "Could frame thy fearful symmetry?"]

main :: IO ()
main =
  print $
  show (myLines sentences '\n')
  ++ " - "
  ++ "Are they equal? "
  ++ show (myLines sentences '\n'
          == shouldEqual)
