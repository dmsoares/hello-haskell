module Ch13Exercises where

import           Data.Either
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStr "Insert name: "
  name <- getLine
  putStr "Insert age: "
  age <- getLine
  showResult $ mkPerson name $ read age

showResult :: Either PersonInvalid Person -> IO ()
showResult (Left err) = do
  putStrLn $ "An error has occurred: " ++ show err
showResult (Right p) = do
  putStrLn $ "Yay! Successfully got a person: " ++ show p
