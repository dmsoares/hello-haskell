-- | Fix the following module

module Print3Broken where

printSecond :: String -> IO ()
printSecond str = do
  putStrLn str

printThird :: IO ()
printThird = do
  putStrLn greeting

greeting :: String
greeting = "Yarrrrr"

main :: IO ()
main = do
  putStrLn greeting
  printThird
  printSecond greeting
  where greeting = "Yarrrrr"
