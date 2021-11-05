-- |

module Ch12SignalingAdversity where

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
  if even n then Just (n+2) else Nothing

-- Smart constructors for datatypes
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 = Just $ Person name age
  | otherwise = Nothing

-- ...but what if we want to know which attribute (name or age)
-- impeded the construction of a Person?

-- We use Either
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name == "" = Left NameEmpty
  | age < 0 = Left AgeTooLow
  | otherwise = Right $ Person name age

-- ...we want to be able to have feedback on which errors,
-- if more than one, prevented the construction of a Person
type ValidatePerson a =
  Either [PersonInvalid] a

ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> ValidatePerson Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPerson'' :: Name -> Age -> ValidatePerson Person
mkPerson'' name age =
  mkPerson''' (nameOkay name) (ageOkay age)

mkPerson''' :: ValidatePerson Name
            -> ValidatePerson Age
            -> ValidatePerson Person
mkPerson''' (Right nameOk) (Right ageOk) = Right $ Person nameOk ageOk
mkPerson''' (Left badName) (Left badAge) = Left $ badName ++ badAge
mkPerson''' (Left badName) _ = Left badName
mkPerson''' _ (Left badAge) = Left badAge
