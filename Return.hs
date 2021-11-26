-- |

module Return where

twoo :: IO ()
twoo = do c   <- getChar
          c'  <- getChar
          if c == 'a'
          then return ()
          else putStrLn $ "\nResult = " ++ show (c == c')
