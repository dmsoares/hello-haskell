module Ch18Monad where

import           Control.Applicative ((*>))
import           Control.Monad       (join)

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
  putStrLn "blah"
    >> putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "blah"
    *> putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= \name ->
    putStr "greeting: "
      >> putStrLn ("hi " ++ name)

binding'' :: IO ()
binding'' =
  -- join $ putStrLn . ("greeting: hi " ++) <$> getLine
  putStrLn . ("greeting: hi " ++) =<< getLine

bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "name pls:"
    >> getLine
    >>= \name -> putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn
    ( "y helo thar: "
        ++ name
        ++ " who is: "
        ++ age
        ++ " years old."
    )

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name pls:"
    >> getLine
    >>= \name ->
      putStrLn "age pls"
        >> getLine
        >>= \age ->
          putStrLn
            ( "y helo thar: "
                ++ name
                ++ " who is: "
                ++ age
                ++ " years old."
            )

-- List Monad in use
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else [x * x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' =
  concatMap (\x -> if even x then [x * x, x * x] else [x * x])

twiceWhenEven'' :: [Integer] -> [Integer]
twiceWhenEven'' xs = do
  x <- xs
  if even x
    then [x * x, x * x]
    else []

concat' :: Eq a => [[a]] -> [a]
concat' [] = []
concat' (x : xs) =
  if null x
    then concat' xs
    else x ++ concat' xs

-- EitherMonad
type Founded = Int

type Coders = Int

data SoftwareShop = Shop
  { founded     :: Founded,
    programmers :: Coders
  }
  deriving (Eq, Show)

data FoundedError
  = NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded ::
  Int ->
  Either FoundedError Founded
validateFounded n
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders ::
  Int ->
  Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware ::
  Int ->
  Int ->
  Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then
      Left $
        TooManyCodersForYears years coders
    else Right $ Shop founded programmers
